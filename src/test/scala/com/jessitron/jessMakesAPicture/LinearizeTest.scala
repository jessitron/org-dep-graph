package com.jessitron.jessMakesAPicture

import org.scalatest.{FlatSpec, Matchers}

class LinearizeTest extends FlatSpec with Matchers {

  it should "put these buggers in order" in {
    val relationships = Seq((5,4), (4,3), (3,2))

    val output = Linearize.tsort(relationships)

    output should be(Seq(5,4,3,2))
  }

  it should "find a build order" in {

    val relationships = List(("rug-cli","artifact-source"),
      ("rug-cli","rug-resolver"),
      ("rug-cli","rug"),
      ("rug-cli","rug-typescript-compiler"),
      ("rug","artifact-source"),
      ("rug","rug-compiler"),
      ("rug","rug-typescript-compiler"),
      ("rug-resolver","rug"),
      ("rug-typescript-compiler","rug-compiler"),
      ("rug-compiler","artifact-source"))

    val output = Linearize.tsort(relationships)

    output.head should be("rug-cli")

  }
}
