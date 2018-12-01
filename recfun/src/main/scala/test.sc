val list = "eoijg()()(((()))".toList
list.collect{
  case '(' => '('
case ')' => ')'}