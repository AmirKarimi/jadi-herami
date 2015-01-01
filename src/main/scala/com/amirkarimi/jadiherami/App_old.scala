package com.amirkarimi.jadiherami

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.annotation._

object App1 {

  private def main(args: Array[String]): Unit = {
    val future = iterateAll(Person(), 13)    
    Await.ready(future, 1 hours)    
  }
  
  def iterateAll(person: Person, ttl: Int): Future[Person] = {
    person.print
    
    if (ttl <= 0) {
      Future.successful(person)
    } else {
      iterate(person) flatMap { person =>
        iterateAll(person, ttl - 1)
      }
    }
  }
  
  def iterate(person: Person): Future[Person] = {
    val children = if (person.age < 6) {
      Seq(Person(), Person())
    } else {
      Nil
    }
    
    val loopFuture = Future {
      person.children.map(iterate)
    }
    
    loopFuture flatMap { iteratedChildrenFutures =>
      Future.sequence(iteratedChildrenFutures) map { iteratedChildren =>
        val childsOfChildren = iteratedChildren.map(_.totalChildrenCount).sum
        val personChildren = iteratedChildren ++ children
    
        person.copy(age = person.age + 1, children = personChildren, totalChildrenCount = personChildren.length + childsOfChildren)
      }
    }
  }

  case class Person(age: Int = 0, children: Seq[Person] = Nil, totalChildrenCount: Int = 0) {
    def print = {
      println(s"Age: $age | Childs: ${children.length} | TotalChild: ${totalChildrenCount + 1}")
    }
  }
}
