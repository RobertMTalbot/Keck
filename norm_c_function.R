#define normalized change function. NEEDS WORK! 
#na.rm = TRUE?
normchange <- function(x, y)
{
  if (y>x) 
  {(y-x)/(100-x)}
  else
  {if (y & x == 100 | y & x == 0)
  {NA}
    else
    {if (y == x)
    {0}
      else
      {if (y<x)
      {(y-x)/x}
      }}}
}
#Warning message:
#In if (y > x) { :
 #   the condition has length > 1 and only the first element will be used

