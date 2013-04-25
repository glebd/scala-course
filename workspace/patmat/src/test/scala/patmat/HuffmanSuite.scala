package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val frenchFreq: List[(Char, Int)] = List(('e', 225947), ('s', 121895), ('a', 117110), ('i', 115465), ('t', 111103),
      ('n', 108812), ('r', 100500), ('u', 96785), ('l', 83668), ('o', 82762), ('d', 56269), ('c', 50003), ('p', 46335),
      ('m', 45521), ('v', 24975), ('q', 20889), ('f', 16351), ('b', 13822), ('g', 13288), ('h', 11298), ('j', 8351),
      ('x', 5928), ('y', 4725), ('z', 2093), ('w', 1747), ('k', 745))
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode b") {
    new TestTrees {
      assert(encode(t2)("b".toList) === List(0, 1))
    }
  }

  test("decode and encode") {
    new TestTrees {
      assert(decode(t2, encode(t2)("abd".toList)) === "abd".toList)
    }
  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quickEncode b") {
    new TestTrees {
      assert(quickEncode(t2)("b".toList) === List(0, 1))
    }
  }

  test("decode and quickEncode") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("abd".toList)) === "abd".toList)
    }
  }

  test("createCodeTree of a very short text") {
    new TestTrees {
      val tree = createCodeTree("aabbb".toList)
      assert(tree === t1)
    }
  }

//  test("createCodeTree of a text") {
//    new TestTrees {
//      val tree = createCodeTree("aabbbdddd".toList)
//      println(t2)
//      println(tree)
//      assert(tree === t2)
//    }
//  }
  
  test("french code") {
    new TestTrees {
      assert(convert(until(singleton, combine)(makeOrderedLeafList(frenchFreq)).head) == convert(frenchCode))
    }
  }
  
  test("assignment instructions tree") {
    val cs = "aaaaaaaabbbcdefgh".toList
    val tree = createCodeTree(cs)
    assert(encode(tree)(cs).size === 41)
  }
  
  test("assignment instructions tree minimal test") {
    val tree = createCodeTree("aaaaaaaabbbcdefgh".toList)
    assert(encode(tree)("abcdefgh".toList).size === 28)
  }
}
