package so.modernized

/**
 * @author John Sullivan
 */
class SplitIterator[A](t:Traversable[A], pred:(A => Boolean)) extends Iterator[Traversable[A]] {
  var _first:Traversable[A] = null
  var _rest:Traversable[A] = t

  def hasNext: Boolean = _rest.nonEmpty && {
    var r = _rest.span(pred)
    _first = r._1; _rest = r._2
    while(_first.isEmpty && _rest.nonEmpty) {
      r = _rest.tail.span(pred)
      _first = r._1; _rest = r._2
    }
    _first.nonEmpty
  }

  def next(): Traversable[A] = {
    if(_first==null) {
      hasNext
    }
    if(_first.isEmpty) {
      throw new NoSuchElementException()
    }
    _first
  }
}