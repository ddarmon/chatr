# Load Required Libraries

library(shiny)
library(DBI)
library(RSQLite)
library(ellmer)
library(markdown)
library(shinyjs)
library(bslib)

# -- Options -------------------------------------------------------------------

# Put the `OLLAMA_BASE_URL` in an `.Renviron` file in the same directory as
# this script.

OLLAMA_BASE_URL = Sys.getenv("OLLAMA_BASE_URL", "http://127.0.0.1:11434")

# -- Helper Functions ----------------------------------------------------------

# -- For Markdown Rendering ----------------------------------------------------

replace_math_delimiters <- function(txt) {
  txt <- gsub("(?s)\\\\\\[\\s*(.*?)\\s*\\\\\\]", "$$\\1$$", txt, perl = TRUE)
  txt <- gsub("(?s)\\\\\\(\\s*(.*?)\\s*\\\\\\)", "$\\1$", txt, perl = TRUE)
  return(txt)
}

markdown_to_html <- function(txt) {
  txt <- replace_math_delimiters(txt)
  html <- markdown::markdownToHTML(
    text = txt,
    fragment.only = TRUE,
    options = c("skip_html", "mathjax", "no_smartypants")
  )
  # This vvv is needed since MathJax will convert `$...$` back to `\(...\)`, and
  # I've set the inline delimiters to be `$...$`.
  html <- gsub("\\\\\\((.*?)\\\\\\)", "$\\1$", html, perl = TRUE)
  html
}

# -- For the Database ----------------------------------------------------------

init_db <- function() {
  con <- tryCatch({
    dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
  }, error = function(e) {
    stop("Failed to connect to the database: ", e$message)
  })

  if (!"conversations" %in% dbListTables(con)) {
    dbExecute(con, "
      CREATE TABLE conversations (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        title TEXT,
        model TEXT,
        created_at TEXT DEFAULT (datetime('now', 'utc'))
      )
    ")
  }
  if (!"messages" %in% dbListTables(con)) {
    dbExecute(con, "
      CREATE TABLE messages (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        conversation_id INTEGER,
        role TEXT,
        content TEXT,
        created_at TEXT DEFAULT (datetime('now', 'utc')),
        FOREIGN KEY(conversation_id) REFERENCES conversations(id)
      )
    ")
  }
  dbDisconnect(con)
}

get_latest_chat_id <- function() {
  con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
  res <- dbGetQuery(
    con,
    "SELECT id FROM conversations ORDER BY created_at DESC LIMIT 1"
  )
  dbDisconnect(con)
  if (nrow(res) > 0) res$id[1] else NULL
}

# -- For Interacting with Chats ------------------------------------------------

create_new_chat <- function(model_name) {
  con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
  tryCatch({
    dbExecute(
      con,
      "INSERT INTO conversations (title, model) VALUES (?, ?)",
      params = list("New Chat", model_name)
    )
    get_latest_chat_id()
  }, error = function(e) {
    showNotification("Error creating new chat", type = "error")
    NULL
  }, finally = {
    dbDisconnect(con)
  })
}

update_chat_title <- function(chat_id, new_title) {
  con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
  dbExecute(
    con,
    "UPDATE conversations SET title = ? WHERE id = ?",
    params = list(new_title, chat_id)
  )
  dbDisconnect(con)
}

# -- For Rendering Chat Messages -----------------------------------------------

render_message <- function(role, content) {
  content_html <- markdown_to_html(content)
  div(
    class = paste("message", paste0(role, "-message")),
    # Add copy button for assistant messages only
    if (role == "assistant") {
      div(
        class = "copy-button",
        tags$button(
          onclick = "copyMessageContent(this)",
          icon("copy"),
          "Copy"
        )
      )
    },
    div(
      class = "message-content",
      `data-raw-content` = content,  # Store raw content as data attribute
      HTML(content_html)
    )
  )
}

# -- UI ------------------------------------------------------------------------

ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    "enable-transitions" = TRUE
  ),

  useShinyjs(),

  # Keep existing head tags
  tags$head(
    # Optimized MathJax v3 configuration
    tags$script(HTML("
      window.MathJax = {
        tex: {
          inlineMath: [['$', '$']],
          displayMath: [['$$', '$$']],
          processEscapes: false,
          tags: 'none'
        },
        options: {
          skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre'],
          'HTML-CSS': {
            preferredFont: 'TeX',
            availableFonts: ['TeX'],
            styles: { '.MathJax_Display': { margin: '0em 0em' } }
          }
        },
        startup: {
          pageReady: function() {
            return MathJax.startup.defaultPageReady({
              renderDelay: 0
            });
          }
        }
      };
    ")),

    # MathJax v3 script
    tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/3.2.2/es5/tex-mml-chtml.js",
      async = NA
    ),

    # Optimized JavaScript for streaming updates
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateStreaming', function(msg) {
        var chatContainer = document.querySelector('.chat-container');
        var streamingDiv = document.querySelector('#streaming-response');

        if (!streamingDiv) {
          streamingDiv = document.createElement('div');
          streamingDiv.id = 'streaming-response';
          streamingDiv.className = 'message assistant-message streaming';
          chatContainer.appendChild(streamingDiv);
        }

        streamingDiv.innerHTML = msg.fullText;

        // Batch MathJax updates using requestAnimationFrame
        if (MathJax.typeset) {
          requestAnimationFrame(function() {
            MathJax.typesetPromise([streamingDiv])
              .catch(function(err) {
                console.error('MathJax typeset failed: ' + err.message);
              });
          });
        }

        // Optimized scrolling using requestAnimationFrame
        requestAnimationFrame(function() {
          chatContainer.scrollTop = chatContainer.scrollHeight;
        });

        // Immediate continuation of streaming
        Shiny.setInputValue('continueStreaming', true, {priority: 'event'});
      });

      Shiny.addCustomMessageHandler('clearStreaming', function(msg) {
        var streamingDiv = document.querySelector('#streaming-response');
        if (streamingDiv) {
          streamingDiv.remove();
        }
      });
    ")),

    # JavaScript for copy button functionality
    tags$script(HTML("
      function copyMessageContent(button) {
        const messageDiv = button.closest('.message');
        const content = messageDiv.querySelector('.message-content').getAttribute('data-raw-content');

        // Try multiple copy methods
        function fallbackCopyTextToClipboard(text) {
          // Create temporary textarea
          const textArea = document.createElement('textarea');
          textArea.value = text;
          textArea.style.position = 'fixed';
          textArea.style.left = '-9999px';
          document.body.appendChild(textArea);

          try {
            textArea.select();
            document.execCommand('copy');
            return true;
          } catch (err) {
            console.error('Fallback copy failed:', err);
            return false;
          } finally {
            document.body.removeChild(textArea);
          }
        }

        // Try modern clipboard API first, then fallback
        const copySuccess = () => {
          const originalText = button.textContent;
          button.textContent = 'Copied!';
          setTimeout(() => {
            button.textContent = originalText;
          }, 2000);
        };

        const copyFail = () => {
          button.textContent = 'Copy failed';
          setTimeout(() => {
            button.textContent = 'Copy';
          }, 2000);
        };

        if (navigator.clipboard && window.isSecureContext) {
          navigator.clipboard.writeText(content)
            .then(copySuccess)
            .catch(() => {
              // Try fallback if clipboard API fails
              if (fallbackCopyTextToClipboard(content)) {
                copySuccess();
              } else {
                copyFail();
              }
            });
        } else {
          // Use fallback for non-HTTPS or unsupported browsers
          if (fallbackCopyTextToClipboard(content)) {
            copySuccess();
          } else {
            copyFail();
          }
        }
      }
    ")),

    # JavaScript for capturing Cmd/Ctrl + Enter with debug logging
    tags$script(HTML("
      $(document).ready(function() {
        $('#user_input').on('keydown', function(e) {
          console.log('Key event:', {
            key: e.key,
            keyCode: e.keyCode,
            metaKey: e.metaKey,
            ctrlKey: e.ctrlKey
          });

          if ((e.metaKey || e.ctrlKey) && (e.keyCode === 13 || e.key === 'Enter')) {
            console.log('Command+Enter detected');
            e.preventDefault();
            $('#send').trigger('click');
            return false;
          }
        });
      });
    ")),

    # JavaScript for capturing Cmd/Ctrl + Enter
    tags$script(HTML("
      $(document).on('keydown', 'textarea[id=\"user_input\"]', function(e) {
        console.log('Keydown event on textarea');
        if ((e.metaKey || e.ctrlKey) && e.keyCode === 13) {
          console.log('Cmd+Enter detected');
          e.preventDefault();
          document.getElementById('send').click();
          return false;
        }
      });
    ")),

    # CSS Styles
    tags$style("
      .chat-container {
        height: calc(100vh - px);
        overflow-y: auto;
        padding: 10px;
        background-color: #fafafa;
        display: flex;
        flex-direction: column;
      }
      .message {
        margin: 10px 0;
        padding: 10px;
        border-radius: 5px;
        max-width: 80%;
        position: relative;  /* Add this */
      }
      .user-message {
        background-color: #f0f0f0;
        align-self: flex-end;
      }
      .assistant-message {
        background-color: #e3f2fd;
        align-self: flex-start;
      }
      .streaming {
        opacity: 0.7;
      }
      .copy-button {
        position: absolute;
        top: 5px;
        right: 5px;
        opacity: 0;
        transition: opacity 0.2s;
      }
      .message:hover .copy-button {
        opacity: 1;
      }
      .copy-button button {
        background: transparent;
        border: 1px solid #ccc;
        border-radius: 4px;
        padding: 2px 8px;
        cursor: pointer;
      }
      .copy-button button:hover {
        background: #f0f0f0;
      }
      a[id^='delete_'] {
        opacity: 0.7;
        text-decoration: none;
      }
      a[id^='delete_']:hover {
        opacity: 1;
        text-decoration: none;
        cursor: pointer;
      }
    "),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
    )
  ),

  # Sidebar
  sidebar = sidebar(
    selectInput("model", "Select Model",
      choices = c("phi4:latest", "qwq:latest", "gemma2:27b", "llama3.2:1b"),
      selected = "phi4:latest"
    ),
    card(
      min_height = "40px",
      class = "p-2 mb-3",
      style = "font-size: 0.85rem;",
      span(class = "text-muted", "Current Model:"),
      textOutput("current_model", container = span)
    ),
    layout_column_wrap(
      width = 1,
      actionButton("new_chat", "New Chat", class = "btn-primary w-100")
    ),
    nav_panel(  # Changed from nav() to nav_panel()
      "Chat History",
      uiOutput("conversation_list")
    )
  ),

  # Restructured main content - remove fixed positioning
  div(
    class = "d-flex flex-column vh-100",

    # Chat messages container with chat-container class
    div(
      id = "chat-messages-container",
      class = "chat-container flex-grow-1 overflow-auto",  # Added chat-container class here
      style = "height: calc(100vh - 180px);",  # Adjust for input card height
      uiOutput("chat_messages")
    ),

    # Input card as part of the flow
    card(
      class = "border-top mt-auto",  # mt-auto pushes it to bottom
      full_screen = FALSE,
      card_body(
        class = "p-3",
        textAreaInput("user_input", "", width = "100%", rows = 3),
        layout_column_wrap(
          width = "150px",
          gap = "10px",
          actionButton("send", "Send", class = "btn-primary w-100"),
          actionButton("stop", "Stop", class = "btn-danger w-100")
        )
      )
    )
  )
)

# -- Server --------------------------------------------------------------------

server <- function(input, output, session) {
  # Initialize DB
  init_db()

  rv <- reactiveValues(
    current_chat = NULL,
    chat_instance = NULL,
    is_streaming = FALSE,
    current_model = "phi4:latest",  # Initialize with default model
    should_stop = FALSE,
    update_conversation_list = 0
  )

  # Add this observe block right after rv initialization
  observe({
    # Trigger initial conversation list load
    rv$update_conversation_list <- isolate(rv$update_conversation_list) + 1
  })

  # Update current model when chat is loaded or changed
  observe({
    if (!is.null(rv$current_chat)) {
      con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
      model <- dbGetQuery(
        con,
        "SELECT model FROM conversations WHERE id = ?",
        params = list(rv$current_chat)
      )$model[1]
      dbDisconnect(con)

      rv$current_model <- model
      updateSelectInput(session, "model", selected = model)
    }
  })

  # Display current model - simplified to respond directly to input
  output$current_model <- renderText({
    input$model  # Add explicit dependency on input$model
  })

  session$userData$stream_fun      <- NULL
  session$userData$response_so_far <- ""
  session$userData$token_buffer    <- character(0)

  # Create new chat
  observeEvent(input$new_chat, {
    rv$chat_instance <- NULL
    rv$current_chat <- NULL

    # First create chat with temporary title
    con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
    tryCatch({
      dbExecute(
        con,
        "INSERT INTO conversations (title, model) VALUES (?, ?)",
        params = list("New Chat", input$model)
      )

      new_chat_id <- get_latest_chat_id()
      rv$current_chat <- new_chat_id
      updateTextAreaInput(session, "user_input", value = "")

      # Reset chat messages
      output$chat_messages <- renderUI({ tagList() })

    }, error = function(e) {
      showNotification("Error creating new chat", type = "error")
    }, finally = {
      dbDisconnect(con)
    })

    # Update conversation list immediately
    rv$update_conversation_list <- runif(1)
  })

  # Modified conversation list with reactive dependency
  output$conversation_list <- renderUI({
    # Force update when needed
    req(rv$update_conversation_list)

    con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
    conversations <- dbGetQuery(con, "SELECT id, title, datetime(created_at) as created_at FROM conversations ORDER BY created_at DESC")
    dbDisconnect(con)

    tagList(
      h4("Chat History"),
      if (nrow(conversations) > 0) {
        lapply(seq_len(nrow(conversations)), function(i) {
          div(
            style = "margin-bottom: 10px; display: flex; justify-content: space-between; align-items: center;",
            actionLink(
              inputId = paste0("chat_", conversations$id[i]),
              label = conversations$title[i],  # Removed date for clarity
              style = if(!is.null(rv$current_chat) && rv$current_chat == conversations$id[i])
                "font-weight: bold; color: #007bff;" else "color: #007bff;"
            ),
            actionLink(
              inputId = paste0("delete_", conversations$id[i]),
              label = icon("trash"),  # Using icon instead of text
              style = "color: #dc3545;"
            )
          )
        })
      } else {
        p("No chats yet.")
      }
    )
  })

  observe({
    chat_inputs <- grep("^chat_", names(input), value = TRUE)
    lapply(chat_inputs, function(chat_input) {
      observeEvent(input[[chat_input]], {
        chat_id <- as.numeric(sub("^chat_", "", chat_input))
        rv$current_chat <- chat_id
        load_chat_messages(chat_id)
        rv$update_conversation_list <- runif(1)
      })
    })
  })

  load_chat_messages <- function(chat_id) {
    con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
    model <- dbGetQuery(
      con,
      "SELECT model FROM conversations WHERE id = ?",
      params = list(chat_id)
    )$model[1]

    msgs <- dbGetQuery(
      con,
      "SELECT role, content FROM messages WHERE conversation_id = ? ORDER BY created_at ASC",
      params = list(chat_id)
    )
    dbDisconnect(con)

    # Create new chat instance
    rv$chat_instance <- chat_ollama(
      model = model,
      base_url = OLLAMA_BASE_URL,
      echo = "none"
    )

    # Convert messages to Turns
    turns <- lapply(seq_len(nrow(msgs)), function(i) {
      Turn(role = msgs$role[i], contents = list(ContentText(msgs$content[i])))
    })

    # Set all turns at once
    rv$chat_instance$set_turns(turns)

    # Render messages
    output$chat_messages <- renderUI({
      req(chat_id)
      tagList(
        lapply(seq_len(nrow(msgs)), function(i) {
          render_message(msgs$role[i], msgs$content[i])
        }),
        tags$script("MathJax.typesetPromise();")
      )
    })
  }

  # Optimized streaming process
  process_stream <- function() {
    if (is.null(session$userData$stream_fun)) return()

    # Check if we should stop
    if (rv$should_stop) {
      save_assistant_response()  # Save the partial response before ending
      end_streaming()
      showNotification("Generation stopped", type = "message")
      return()
    }

    token <- tryCatch({
      session$userData$stream_fun()
    }, error = function(e) {
      showNotification(paste("Error streaming:", e$message), type = "error")
      NULL
    })

    if (is.null(token)) {
      save_assistant_response()  # Save response even on error
      end_streaming()
      return()
    }

    if (token == ".__exhausted__.") {
      save_assistant_response()
      end_streaming()
    } else {
      # Immediate update without buffering
      session$userData$response_so_far <- paste0(session$userData$response_so_far, token)
      partial_html <- markdown_to_html(session$userData$response_so_far)

      session$sendCustomMessage("updateStreaming", list(
        fullText = partial_html,
        chunkComplete = TRUE
      ))
    }
  }

  end_streaming <- function() {
    rv$is_streaming <- FALSE
    rv$should_stop <- FALSE
    shinyjs::enable("send")
    session$userData$stream_fun <- NULL
    session$userData$token_buffer <- character(0)
  }

  save_assistant_response <- function() {
    if (nzchar(session$userData$response_so_far)) {
      # Append " (stopped)" to the response if it was interrupted
      final_response <- if (rv$should_stop) {
        paste0(session$userData$response_so_far, " (stopped)")
      } else {
        session$userData$response_so_far
      }

      con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
      dbExecute(con,
                "INSERT INTO messages (conversation_id, role, content) VALUES (?, 'assistant', ?)",
                params = list(rv$current_chat, final_response)
      )
      dbDisconnect(con)

      # Generate a title based on the chat so far
      con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
      chat_so_far <- dbGetQuery(
        con,
        "SELECT * FROM messages WHERE conversation_id = ?",
        params = list(rv$current_chat)
      )
      dbDisconnect(con)

      # Generate title only **after** first response from the `assistant`
      if (nrow(chat_so_far) == 2) {
        tryCatch({
          # Generate title using phi4 model
          title_chat <- chat_ollama(
            model = "phi4:latest",
            base_url = OLLAMA_BASE_URL,
            echo = "none"
          )

          prompt <- paste0(
            "Based on this chat, generate a brief (3-5 words) title for the conversation:\n```chat\n",
            paste0(chat_so_far$role, ": ", chat_so_far$content, collapse = "\n"), "\n```\n",
            "Just provide the title followed by a new line."
          )
          suggested_title <- title_chat$chat(prompt)
          suggested_title <- strsplit(suggested_title, "\\n")[[1]][1]

          if (nchar(suggested_title) > 0) {
            update_chat_title(rv$current_chat, suggested_title)
            # Trigger immediate conversation list update
            rv$update_conversation_list <- runif(1)
            # Force UI update
            session$sendCustomMessage("clearStreaming", list())
          }
        }, error = function(e) {
          # If title generation fails, keep "New Chat"
          showNotification("Could not generate chat title", type = "warning")
        })
      }

      session$sendCustomMessage("clearStreaming", list())

      # Re-render entire chat
      output$chat_messages <- renderUI({
        req(rv$current_chat)
        con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
        msgs <- dbGetQuery(
          con,
          "SELECT role, content FROM messages WHERE conversation_id = ? ORDER BY created_at ASC",
          params = list(rv$current_chat)
        )
        dbDisconnect(con)

        tagList(
          lapply(seq_len(nrow(msgs)), function(i) {
            render_message(msgs$role[i], msgs$content[i])
          }),
          tags$script("MathJax.typesetPromise();")
        )
      })

      session$userData$response_so_far <- ""
    }
  }

  # Streaming observer
  observeEvent(input$continueStreaming, {
    if (input$continueStreaming && rv$is_streaming) {
      process_stream()
    }
  })

  # Initialize button states
  observe({
    shinyjs::toggleState("stop", condition = rv$is_streaming)
    shinyjs::toggleState("send", condition = !rv$is_streaming)
  })

  # Handle stop button
  observeEvent(input$stop, {
    rv$should_stop <- TRUE
    showNotification("Stopping generation...", type = "message")
  })

  # Send messages
  observeEvent(input$send, {
    req(input$user_input, input$user_input != "")
    req(!rv$is_streaming)

    # Create new chat if none exists
    if (is.null(rv$current_chat)) {
      new_chat_id <- create_new_chat(input$model)
      if (!is.null(new_chat_id)) {
        rv$current_chat <- new_chat_id
        rv$update_conversation_list <- runif(1)
      } else {
        return()  # Exit if chat creation failed
      }
    }

    rv$should_stop <- FALSE
    user_txt <- input$user_input
    updateTextAreaInput(session, "user_input", value = "")

    con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
    dbExecute(con,
              "INSERT INTO messages (conversation_id, role, content) VALUES (?, 'user', ?)",
              params = list(rv$current_chat, user_txt)
    )
    dbDisconnect(con)

    # Re-render static chat with user message
    output$chat_messages <- renderUI({
      req(rv$current_chat)
      con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
      msgs <- dbGetQuery(
        con,
        "SELECT role, content FROM messages WHERE conversation_id = ? ORDER BY created_at ASC",
        params = list(rv$current_chat)
      )
      dbDisconnect(con)

      tagList(
        lapply(seq_len(nrow(msgs)), function(i) {
          render_message(msgs$role[i], msgs$content[i])
        }),
        tags$script("MathJax.typesetPromise();")
      )
    })

    # Begin streaming
    rv$is_streaming <- TRUE
    shinyjs::disable("send")

    session$userData$response_so_far <- ""
    session$userData$token_buffer    <- character(0)

    if (is.null(rv$chat_instance)) {
      rv$chat_instance <- chat_ollama(
        model = input$model,
        base_url = OLLAMA_BASE_URL,
        echo = "none"
      )
    }

    session$userData$stream_fun <- rv$chat_instance$stream(user_txt)
    process_stream()
  })

  # Replace the delete observer with a single parameterized observeEvent
  observeEvent(input$delete_action, {
    # Get the chat ID from the input value
    chat_id <- as.numeric(input$delete_action)

    # Delete from database
    con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
    tryCatch({
      # First delete messages
      dbExecute(con,
               "DELETE FROM messages WHERE conversation_id = ?",
               params = list(chat_id)
      )
      # Then delete conversation
      dbExecute(con,
               "DELETE FROM conversations WHERE id = ?",
               params = list(chat_id)
      )

      # If deleted chat was current chat, clear it
      if (!is.null(rv$current_chat) && rv$current_chat == chat_id) {
        rv$current_chat <- NULL
        rv$chat_instance <- NULL
        output$chat_messages <- renderUI({ tagList() })
      }

      # Update conversation list
      rv$update_conversation_list <- runif(1)

      showNotification("Chat deleted", type = "message")
    }, error = function(e) {
      showNotification("Error deleting chat", type = "error")
    }, finally = {
      dbDisconnect(con)
    })
  }, ignoreInit = TRUE)

  # Modify the conversation list UI to use the new delete mechanism
  output$conversation_list <- renderUI({
    req(rv$update_conversation_list)

    con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
    conversations <- dbGetQuery(con, "SELECT id, title, datetime(created_at) as created_at FROM conversations ORDER BY created_at DESC")
    dbDisconnect(con)

    tagList(
      h4("Chat History"),
      if (nrow(conversations) > 0) {
        lapply(seq_len(nrow(conversations)), function(i) {
          div(
            style = "margin-bottom: 10px; display: flex; justify-content: space-between; align-items: center;",
            actionLink(
              inputId = paste0("chat_", conversations$id[i]),
              label = conversations$title[i],
              style = if(!is.null(rv$current_chat) && rv$current_chat == conversations$id[i])
                "font-weight: bold; color: #007bff;" else "color: #007bff;"
            ),
            # Modified delete button to use new mechanism
            actionButton(
              inputId = paste0("delete_", conversations$id[i]),
              label = NULL,
              icon = icon("trash"),
              class = "btn-link text-danger p-0",
              style = "color: #dc3545;",
              onclick = sprintf("Shiny.setInputValue('delete_action', %d)", conversations$id[i])
            )
          )
        })
      } else {
        p("No chats yet.")
      }
    )
  })

  # Handle model changes - update to respond to all model changes
  observeEvent(input$model, {
    # Always update current_model to match dropdown
    rv$current_model <- input$model

    # Only update database if there's an active chat
    if (!is.null(rv$current_chat)) {
      con <- dbConnect(RSQLite::SQLite(), "chat_history.sqlite")
      dbExecute(
        con,
        "UPDATE conversations SET model = ? WHERE id = ?",
        params = list(input$model, rv$current_chat)
      )
      dbDisconnect(con)

      # Reset chat instance to force new model
      rv$chat_instance <- NULL

      # Show notification about model change
      showNotification(
        paste("Switched to model:", input$model),
        type = "message"
      )
    }
  })
}

# Launch Shiny app

shinyApp(ui = ui, server = server)