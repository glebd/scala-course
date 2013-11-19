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
    
    val airTraffic = false
    val airTrafficProbability = 0.01
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
    
    afterDelay(0)(mode)
    
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

      if (airTraffic && (randomBelow(100) <= airTrafficProbability*100)) {
        // random room
        moveTo(randomBelow(roomRows), randomBelow(roomColumns))
      } else {
        println(s"\nThis room: ($row, $col)")
        
        // determine available rooms
        val pr = prevRow(row)
        val topRoom = List(pr, col)
        println(s"Top room: ($pr, $col)");
        
        val nc = nextCol(col)
        val rightRoom = List(row, nc)
        println(s"Right room: ($row, $nc)")
        
        val nr = nextRow(row)
        val bottomRoom = List(nr, col)
        println(s"Bottom room: ($nr, $col)")
        
        val pc = prevCol(col)
        val leftRoom = List(row, pc)
        println(s"Left room: ($row, $pc)")

        val rooms = List(topRoom, rightRoom, bottomRoom, leftRoom)
        val safeRooms = rooms.filter(roomAppearsSafe)
        if (safeRooms != Nil) {
          val roomIndex = randomBelow(safeRooms.length)
          val room = safeRooms(roomIndex)
          moveTo(room(0), room(1))
          println(s"Moving to room (${room(0)},${room(1)})")
        } else {
          println("No safe rooms:")
          println("  top     - " + (if (roomAppearsSafe(topRoom)) "safe" else dumpRoom(topRoom)))
          println("  right   - " + (if (roomAppearsSafe(rightRoom)) "safe" else dumpRoom(topRoom)))
          println("  bottom  - " + (if (roomAppearsSafe(bottomRoom)) "safe" else dumpRoom(topRoom)))
          println("  left    - " + (if (roomAppearsSafe(leftRoom)) "safe" else dumpRoom(topRoom)))
          println("Staying put")
        }
      }

      mode
    }
    
    def dumpFlags(): String = {
      "{ " + (if (infected) "inf " else "") + (if (sick) "sick " else "") + (if (immune) "imm " else "") + (if (dead) "dead " else "") + "}"
    }
    
    def dumpRoom(coords: List[Int]): String = {
      val s = new StringBuilder
      persons
        .filter(p => p.row == coords(0) && p.col == coords(1))
        .foreach(p => s.append(p.dumpFlags()))
      s.mkString
    }
    
    def moveTo(newrow: Int, newcol: Int) {
      row = newrow
      col = newcol
    }
    
    def roomAppearsSafe(coords: List[Int]): Boolean = {
      val row = coords(0)
      val col = coords(1)
      val unsafe = persons.exists(p => {p.row == row && p.col == col && (p.sick || p.dead)})
      return !unsafe
    }
    
    def roomIsSafe(coords: List[Int]): Boolean = {
      val row = coords(0)
      val col = coords(1)
      val unsafe = persons.exists(p => {p.row == row && p.col == col && (p.infected || p.sick || p.dead)})
      return !unsafe
    }
    
    def becomeInfected() {
      if (immune || dead || sick) return
      infected = true
      afterDelay(incubation)(becomeSick)
    }
    
    def becomeSick() {
      sick = true
      afterDelay(death-incubation)(dieMaybe)
    }
    
    def dieMaybe() {
      if (randomBelow(100) <= mortality*100) {
        dead = true
        sick = false
        immune = false
      }
      else afterDelay(immunity-death)(becomeImmune)
    }
    
    def becomeImmune() {
      immune = true
      sick = false
      afterDelay(convalescence-immunity)(becomeHealthy)
    }
    
    def becomeHealthy() {
      sick = false
      immune = false
      infected = false
    }
  }
}
