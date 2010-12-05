################################################################
# Implementation of IdemShell commands and tests.

function idem_CHOWN {
  local user="${2%:*}"
  local group="${2#*:}"
  chown "$user" "$1"
  chown :"$group" "$1"
}
function idem_CHMOD {
  chmod "$2" "$1"
}
function idem_RM {
  rm -rf "$1"
}
function idem_CP {
  { which rsync && rsync "$1" "$2" ;} || cp "$1" "$2"
}
function idem_LNs {
  { [ -L "$2" ] && rm -f "$2" ;} || rm -rf "$2"
  ln -s "$1" "$2"
}
function idem_TOUCH {
  rm -rf "$1"
  touch "$1"
}
function idem_MKDIR {
  rm -rf "$1"
  mkdir -p "$1"
}
function idem_USERADD {
  idem_USERDEL "$1"
  useradd "$@" 
}
function idem_USERDEL {
  getent passwd "$1" && userdel "$1"
}
function idem_GROUPADD {
  idem_GROUPDEL "$1"
  groupadd "$@" 
}
function idem_GROUPDEL {
  getent group "$1" && groupdel "$1"
}
function idem_GPASSWDa {
  gpasswd "$1" -a "$2" 1>/dev/null
}
function idem_GPASSWDd {
  gpasswd "$1" -d "$2" 1>/dev/null
}

function idem_LSo {
  local user="${2%:*}"
  local group="${2#*:}"
  { [ "$user" = "" ]  || idem_helper_LSo "$1" ":$user"  } &&
  { [ "$group" = "" ] || idem_helper_LSo "$1" "$group:" }
}
function idem_helper_LSo {
  local awk_script
  local name
  case "$2" in
    *:) name="${2%:}" ; awk_script='{print $3}' ;;
    :*) name="${2#:}" ; awk_script='{print $4}' ;;
    *)  ! echo 'Mysterious invalid call to LSo helper.' 1>&2 ;;  
  esac
  local normed="${name#+}"
  if [ "$name" = "$normed" ]  # Determine if we are using numric form.
  then
    ls -ld "$1"
  else
    ls -nd "$1"
  fi | awk "$awk_script" | fgrep -x -- "$normed"
}
function idem_LSm {
  ls -ld "$1" | awk '{print $1}' | egrep ".$2"
}
function idem_DASHe {
  [ -e "$1" ]
}
function idem_DASH_ {
  [ "$1" "$2" ]
}
function idem_DIFFq {
  diff -q "$1" "$2" 2>/dev/null
}
function idem_LSl {
  [ `readlink -- "$2"` = "$1" ]
}
function idem_GETENTu {
  getent passwd "$1"
}
function idem_GETENTg {
  getent group "$1"
}
function idem_GROUPS {
  groups -- "$1" | xargs printf '%s\n' | sed '1,2 d' | fgrep -x -- "$2"
}
function idem_Not {
  ! echo 'Unimplemented IdemShell primitive should not be called!' 1>&2
}
function idem_And {
  ! echo 'Unimplemented IdemShell primitive should not be called!' 1>&2
}
function idem_Or {
  ! echo 'Unimplemented IdemShell primitive should not be called!' 1>&2
}
function idem_TRUE {
  ! echo 'Unimplemented IdemShell primitive should not be called!' 1>&2
}
function idem_FALSE {
  ! echo 'Unimplemented IdemShell primitive should not be called!' 1>&2
}

