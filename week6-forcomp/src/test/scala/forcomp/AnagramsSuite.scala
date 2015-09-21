package forcomp

import forcomp.Anagrams._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }



  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }



  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }



  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }



  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    assert(subtract(lard, r) === lad)
  }

  test("mergeOccurrences") {
    val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val y = List(('r', 1), ('v', 2))
    val exp = List(('a', 1), ('d', 1), ('l', 1), ('r', 2), ('v', 2))
    assert(mergeOccurrences(x, y) === exp)
  }


  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("sentence anagrams: yes man") {
    val sentence = List("yes", "man")
    val anagr = sentenceAnagrams(sentence)
    anagr.foreach(println)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  test("comb2 a3") {
    val sentOccur = List(('a', 3))
    val comb = combinations(sentOccur).filter(x => x.nonEmpty)
    val exp = Set(
      List(List(('a', 1)), List(('a', 1)), List(('a', 1))),
      List(List(('a', 1)), List(('a', 2))),
      List(List(('a', 2)), List(('a', 1))),
      List(List(('a', 3)))
    )
    val ana = comb2(sentOccur, comb, Nil, Nil)
    println(ana)
    val anaSet = ana.toSet
    assert(exp === anaSet)
  }

  test("comb2 a1 b1") {
    val sentOccur = List(('a', 2), ('b', 2))
    val comb = combinations(sentOccur).filter(x => x.nonEmpty)
    println(comb)
    val exp = Set(
      List(List(('a',2), ('b',2))),
      List(List(('b',1)), List(('a',2), ('b',1))),
      List(List(('b',2)), List(('a',2))),
      List(List(('b',1)), List(('b',1)), List(('a',2))),
      List(List(('a',1)), List(('a',1), ('b',2))),
      List(List(('a',1), ('b',1)), List(('a',1), ('b',1))),
      List(List(('b',1)), List(('a',1)), List(('a',1), ('b',1))),
      List(List(('a',1)), List(('b',1)), List(('a',1), ('b',1))),
      List(List(('a',1), ('b',2)), List(('a',1))),
      List(List(('b',1)), List(('a',1), ('b',1)), List(('a',1))),
      List(List(('b',2)), List(('a',1)), List(('a',1))),
      List(List(('b',1)), List(('b',1)), List(('a',1)), List(('a',1))),
      List(List(('a',1)), List(('b',2)), List(('a',1))),
      List(List(('a',1), ('b',1)), List(('b',1)), List(('a',1))),
      List(List(('b',1)), List(('a',1)), List(('b',1)), List(('a',1))),
      List(List(('a',1)), List(('b',1)), List(('b',1)), List(('a',1))),
      List(List(('a',2)), List(('b',2))),
      List(List(('a',1)), List(('a',1)), List(('b',2))),
      List(List(('a',2), ('b',1)), List(('b',1))),
      List(List(('b',1)), List(('a',2)), List(('b',1))),
      List(List(('a',1)), List(('a',1), ('b',1)), List(('b',1))),
      List(List(('a',1), ('b',1)), List(('a',1)), List(('b',1))),
      List(List(('b',1)), List(('a',1)), List(('a',1)), List(('b',1))),
      List(List(('a',1)), List(('b',1)), List(('a',1)), List(('b',1))),
      List(List(('a',2)), List(('b',1)), List(('b',1))),
      List(List(('a',1)), List(('a',1)), List(('b',1)), List(('b',1)))
    )
    val ana = comb2(sentOccur, comb, Nil, Nil)
    println(ana)
    val anaSet = ana.toSet
    assert(exp === anaSet)
  }

}
