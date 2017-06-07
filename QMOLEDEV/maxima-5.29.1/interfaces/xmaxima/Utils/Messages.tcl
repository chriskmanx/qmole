
proc M {str args} {
    if {$args == ""} {return $str}
    return [eval [list format $str] $args]
}
