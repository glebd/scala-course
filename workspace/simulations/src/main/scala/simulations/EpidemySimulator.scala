package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    
    val prevalence = 0.01
    val incubation = 6
    val death = 14
    val immunity = 16
    val convalescence = 18
    val mortality = 0.25
    val transmissibility = 0.4
    val daysBeforeMove = 5
  }

  import SimConfig._

  val persons: List[Person] = (for (i <- 0 to population) yield new Person(i)).toList

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    if ((id + 1) % 100 == 0)
      becomeInfected
    
    afterDelay(1)(mode)
    
    def mode() {
      val days = randomBelow(daysBeforeMove - 1) + 1
      afterDelay(days)(move)
    }
    
    def move {
      
    }
    
    def becomeInfected() {
      if (immune || dead || sick) return
      infected = true
      afterDelay(incubation)(becomeSick)
      if (randomBelow(100) <= mortality*100)
        afterDelay(death)(die)
      else {
        afterDelay(immunity)(becomeImmune)
        afterDelay(convalescence)(becomeHealthy)
      }
    }
    
    def becomeSick() {
      if (!infected) return
      sick = true
    }
    
    def die() {
      if (!sick) return
      dead = true
    }
    
    def becomeImmune() {
      if (!sick) return
      immune = true
    }
    
    def becomeHealthy() {
      if (sick) return
      sick = false
      immune = false
      infected = false
    }
  }
}
