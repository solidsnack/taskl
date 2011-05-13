{-| Example Task\\L library for working with autotools.
 -}

import System.Directory



--  Creates a Task\L module with autotools:configure defined in it.
configure                   ::  Var FilePath -> TaskL
configure directory          =  "autotools:configure" -# do
  Bash -? "[ -e \"$1\"/config.status ]"
  Exec -! directory ["./configure"]

--  Creates a Task\L module with autotools:make defined as well as
--  autotools:configure.
make                        ::  Var FilePath -> TaskL
make directory               =  "autotools:make" -# do
  Exec -? ["make", "-C", directory, "--question", 1 -$]
  Exec -! ["make", "-C", directory, "--jobs=4", 1 -$]
  configure directory

