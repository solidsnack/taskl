
{-| Pretty printer for Bash. The pretty printer produces readable code where
 
 *  A @do@ always follows @for@ on a separate line (ditto for @then@ and
    @if@).

 *  Lines (but not words) are broken opportunistically to stay under 79
    columns.

 -}

module System.TaskL.Bash.PrettyPrinter where


{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  Line break algorithm for simple commands and arguments of for loops:

    IF the width of the current line with the given word added is greater
       than 79 columns
    THEN
      IF moving the word to the following line causes the line to be shorter
      THEN
        do it
      ELSE
        add the current word to the current line
      DONE
    ELSE
      add the current word to the current line
    DONE

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

