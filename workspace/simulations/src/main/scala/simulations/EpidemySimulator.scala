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

    // Initial incidence
    if ((id + 1) % 100 == 0)
      becomeInfected
    
    afterDelay(1)(mode)
    
    def mode() {
      getInfectedMaybe()
      val days = randomBelow(daysBeforeMove - 1) + 1
      afterDelay(days)(move)
    }
    
    // Check the room
    def getInfectedMaybe() {
      if (infected || sick || dead || immune) return
      if (roomIsSafe(List(row, col))) return
      if (randomBelow(100) <= transmissibility*100)
        afterDelay(0)(becomeInfected)
    }
    
    def prevRow(r: Int): Int = {
      if (r == 0) roomRows-1
      else r-1
    }
    
    def prevCol(c: Int): Int = {
      if (c == 0) roomColumns-1
      else c-1
    }
    
    def nextRow(r: Int): Int = {
      if (r == roomRows-1) 0
      else r+1
    }
    
    def nextCol(c: Int): Int = {
      if (c == roomColumns-1) 0
      else c+1
    }
    
    def move {
      if (dead) return
      
      // determine available rooms
      val topRoom = List(prevRow(row), col)
      val rightRoom = List(row, nextCol(col))
      val bottomRoom = List(nextRow(row), col)
      val leftRoom = List(row, prevCol(col))
      
      val rooms = List(topRoom, rightRoom, bottomRoom, leftRoom)
      val safeRooms = rooms.filter(roomAppearsSafe)
      if (safeRooms != Nil) {
        val roomIndex = randomBelow(safeRooms.length)
        val room = safeRooms(roomIndex)
        moveTo(room(0), room(1))
      }
      
      afterDelay(0)(mode)
    }
    
    def moveTo(newrow: Int, newcol: Int) {
      row = newrow
      col = newcol
    }
    
    def roomAppearsSafe(coords: List[Int]): Boolean = {
      val row = coords(0)
      val col = coords(1)
      val unsafe = persons.exists(p => {p.sick || p.dead})
      return !unsafe
    }
    
    def roomIsSafe(coords: List[Int]): Boolean = {
      val row = coords(0)
      val col = coords(1)
      val unsafe = persons.exists(p => {p.infected || p.sick || p.dead})
      return !unsafe
    }
    
    def becomeInfected() {
      if (immune || dead || sick) return
      infected = true
      afterDelay(incubation)(becomeSick)
    }
    
    def becomeSick() {
      if (!infected) return
      sick = true
      afterDelay(death-incubation)(dieMaybe)
    }
    
    def dieMaybe() {
      if (!sick) return
      if (randomBelow(100) <= mortality*100) dead = true
      else afterDelay(immunity-death)(becomeImmune)
    }
    
    def becomeImmune() {
      if (!sick) return
      immune = true
      afterDelay(convalescence-immunity)(becomeHealthy)
    }
    
    def becomeHealthy() {
      if (sick) return
      sick = false
      immune = false
      infected = false
    }
  }
}
