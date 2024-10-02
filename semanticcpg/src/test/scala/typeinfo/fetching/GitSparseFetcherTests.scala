package io.shiftleft.semanticcpg.typeinfo.fetching

import io.shiftleft.semanticcpg.typeinfo._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Using

/** TODO: static test repo for testing */
class GitSparseFetcherTests extends AnyWordSpec with Matchers {
  "getVersions" should {
    "return all versions for a package" in {
      Using.resource(GitSparseFetcher()) { fetcher =>
        val versionsInfo = fetcher.fetchVersionsInfo(PackageIdentifier(LanguagePlatform.JVM, "ion-java"))
        val expectedBytes = "[\"v1.0.0\", \"v1.11.9\"]".getBytes("UTF-8")
        versionsInfo.readAllBytes() shouldEqual expectedBytes
      }
    }
  }
}
