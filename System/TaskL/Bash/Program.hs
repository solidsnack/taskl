{-# LANGUAGE EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
  #-}

module System.TaskL.Bash.Program where


{-| Terms that can be combined with one another.
@
  <term>                     =  <simple command>
                             |  ! <term>
                             |  <term> && <term>
                             |  <term> || <term>
                             |  <term> | <term>
                             |  <term> ; <term>
                             |  { <term>+ ;}
                             |  ( <term>+ )
                             |  if <term> then <term>+ else <term>+
                             |  if <term> then <term>+
@
 -}
data Term


newtype SimpleCommand = SimpleCommand [ByteString]

