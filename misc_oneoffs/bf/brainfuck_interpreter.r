brainfuck <- setRefClass(
  "brainfuck",
  fields = list(
    STORAGE_SIZE = "integer",
    storage      = "raw",
    position     = "integer",
    commands     = "character",
    debug        = "logical",
    prompt       = "character"
  ),
  methods = list(
    initialize = function(debug = FALSE)
    {
      debug <<- debug
      STORAGE_SIZE <<- 30000L
      commands <<- c(
        ">" = ".self$increment_position()",
        "<" = ".self$decrement_position()",
        "+" = ".self$increment_value()",
        "-" = ".self$decrement_value()",
        "." = ".self$write_char()",
        "," = ".self$read_char()",
        "[" = "while(storage[position] != raw(1)) {",
        "]" = "}"
      )
    },
    interpret = function(program, prompt = "")
    {
      position <<- 1L          
      storage <<- raw(STORAGE_SIZE) 
      prompt <<- prompt
      program <- prepare_program(program)
      converted <- paste(commands[program], collapse = "\n")   
      eval(parse(text = converted), .self)
      invisible(converted)
    },
    prepare_program = function(program)
    {
      program <- paste(program, collapse = "")
      program <- gsub("[^][<>+.,-]", "", program)
      strsplit(program, "")[[1]]
    },
    increment_position = function()
    {
      if(position == STORAGE_SIZE) 
      {
        stop("You have moved past the end of the array.")          
      }
      new_position <- position + 1L
      if(debug) 
      {
        message("Incrementing position to ", new_position)
      }
      position <<- new_position
    },
    decrement_position = function()
    {
      if(position == 1) 
      {
        stop("You have moved past the beginning of the array.")          
      }
      new_position <- position - 1L
      if(debug) 
      {
        message("Decrementing position to ", new_position)
      }
      position <<- new_position
    },
    increment_value = function()
    {
      new_value <- as.integer(storage[position]) + 1L
      if(new_value > 255L)
      {
        stop("New value at position ", position, " is more than 255.")
      }
      if(debug) 
      {
        message("Incrementing value at position ", position, " to ", new_value)
      }     
      storage[position] <<- as.raw(new_value)
    },
    decrement_value = function()
    {
      new_value <- as.integer(storage[position]) - 1L
      if(new_value < 0L)
      {
        stop("New value at position ", position, " is less than 0")
      }
      if(debug) 
      {
        message("Decrementing value at position ", position, " to ", new_value)
      }     
      storage[position] <<- as.raw(new_value)
    },
    write_char = function()
    {
      cat(rawToChar(storage[position]))
    },
    read_char = function()
    {
      user_input <- readline(prompt = prompt)
      if(nchar(user_input) == 0L)
      {
        stop("No user input.  Exiting program.", call. = FALSE)
      }
      storage[position] <<- charToRaw(substring(user_input, 1L, 1L))
    }
  )    
)

#' Creates a brainfuck interpreter.
#' 
#' Creates an instance of an "brainfuck" reference class.
#' 
#' @param debug If code{TRUE}, extra debugging information is displayed 
#' to the console.
#' @note
#' To use, call\code{fuckbrain} once to create the interpreter, then
#' call the \code{interpret} method on each object that you want to
#' interpret.
#' The code{interpret} method accepts a brainfuck program as a character
#' vector.  Non-brainfuck commands are silently stripped first, so you
#' can include comments in the program, as long as they don't contain
#' \code{<>+-.,[]}.  The optional \code{prompt} argument sets the prompt
#' for reading characters.
#' @examples
#' \dontrun{
#' hello_world <- "+++++ +++++ initialize counter (cell #0) to 10
#' [                           use loop to set the next four cells 
#'                             to 70/100/30/10
#'     > +++++ ++              add  7 to cell #1
#'     > +++++ +++++           add 10 to cell #2 
#'     > +++                   add  3 to cell #3
#'     > +                     add  1 to cell #4
#'     <<<< -                  decrement counter (cell #0)
#' ]                   
#' > ++ .                  print 'H'
#' > + .                   print 'e'
#' +++++ ++ .              print 'l'
#' .                       print 'l'
#' +++ .                   print 'o'
#' > ++ .                  print ' '
#' << +++++ +++++ +++++ .  print 'W'
#' > .                     print 'o'
#' +++ .                   print 'r'
#' ----- - .               print 'l'
#' ----- --- .             print 'd'
#' > + .                   print '!'
#' > .                     print '\n'"  
#' bfi <- fuckbrain()
#' bfi$interpret(hello_world)
#' rot13 <- ",+[                Read first character and start outer 
#'                                character reading loop
#'     -[                       Skip forward if character is 0
#'         >>++++[>++++++++<-]  Set up divisor (32) for division loop
#'                              (MEMORY LAYOUT: dividend copy remainder divisor
#'                                 quotient zero zero)
#'         <+<-[                Set up dividend (x minus 1) and enter 
#'                                division loop
#'             >+>+>-[>>>]      Increase copy and remainder / reduce divisor / 
#'                                Normal case: skip forward
#'             <[[>+<-]>>+>]    Special case: move remainder back to divisor and
#'                                increase quotient
#'             <<<<<-           Decrement dividend
#'         ]                    End division loop
#'     ]>>>[-]+                 End skip loop; 0 former divisor and reuse space
#'                                for a flag
#'     >--[-[<->+++[-]]]<[      Zero that flag unless quotient was 2 or 3; 
#'                                zero quotient; check flag
#'         ++++++++++++<[       If flag then set up divisor (13) for second 
#'                                division loop
#'                                (MEMORY LAYOUT: zero copy dividend divisor 
#'                                remainder quotient zero zero)
#'             >-[>+>>]         Reduce divisor; Normal case: increase remainder
#'             >[+[<+>-]>+>>]   Special case: increase remainder / move it back 
#'                                to divisor / increase quotient
#'             <<<<<-           Decrease dividend
#'         ]                    End division loop
#'         >>[<+>-]             Add remainder back to divisor to get a useful 13
#'         >[                   Skip forward if quotient was 0
#'             -[               Decrement quotient and skip forward if quotient
#'                                was 1
#'                 -<<[-]>>     0 quotient and divisor if quotient was 2
#'             ]<<[<<->>-]>>    0 divisor and subtract 13 from copy if quotient
#'                                was 1
#'         ]<<[<<+>>-]          0 divisor and add 13 to copy if quotient was 0
#'     ]                        End outer skip loop (jump to here if ((character 
#'                                minus 1)/32) was not 2 or 3)
#'     <[-]                     Clear remainder from first division if second 
#'                                division was skipped
#'     <.[-]                    Output ROT13ed character from copy and clear it
#'     <,+                      Read next character
#' ]" 
#' bfi$interpret(rot13)
#' }
#' @references The Hello World program is taken from this Wikipedia page
#' \url{http://en.wikipedia.org/wiki/Brainfuck#Hello_World.21}
#' There are loads of example programs to try in the brainfuck archive.
#' \url{http://esoteric.sange.fi/brainfuck/bf-source/}
#' @export
fuckbrain <- function(debug = FALSE)
{
  invisible(brainfuck$new(debug))
}
