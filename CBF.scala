object CBF
{
  import scala.util.hashing.MurmurHash3

  // Exeplary class Employee
  class Employee (name: String, surname : String, id : Int)
  {
    val Name : String = name
    val Surname : String = surname
    val ID : Int = id

    def == (that: Employee): Boolean =
    {
      if ((Name == that.Name) && (Surname == that.Surname) && (ID == that.ID)) true
      else false
    }
  }

  // CBF implementation
  class CBF(A: LazyList[Employee], m: Int, k: Int)
  {
    var buffer: Array[Int] = new Array[Int](m)
    for(i <- 0 until m)
    {
      buffer(i) = 0
    }

    A.foreach
    {
      x=>
        {
          for(i <- 0 until k)
          {
            buffer((MurmurHash3.stringHash(x.ID.toString, i) % m).abs) += 1
          }
        }
    }

    def add(x: Employee): Unit =
    {
      for(i <- 0 until k)
      {
        buffer((MurmurHash3.stringHash(x.ID.toString, i) % m).abs) += 1
      }
    }

    def delete(x: Employee): Unit =
    {
      for(i <- 0 until k)
      {
        buffer((MurmurHash3.stringHash(x.ID.toString, i) % m).abs) -= 1
      }
    }

    def check(x: Employee): Unit =
    {
      var error: Boolean = false
      for(i <- 0 until k)
      {
        if (buffer((MurmurHash3.stringHash(x.ID.toString, i) % m).abs) == 0)
        {
          error = true
        }
      }
      if(error) println("The employee does not work here!")
      else println("The employee works here!")
    }

  }

  // Program example
  def main(args:Array[String]): Unit =
  {
    val emp1 = new Employee("Filip", "Jaszczyk", 123)
    val emp2 = new Employee("Jan", "Kowalski", 111)
    val emp3 = new Employee("Anna", "Nowak", 101)
    val emp4 = new Employee("Julia", "Nowak", 212)
    val emp5 = new Employee("Jan", "Nowak", 916)

    val arg = emp1 #:: emp2 #:: emp3 #:: emp4 #:: LazyList.empty

    var Filter = new CBF(arg, 100, 10)

    println("Checking if Jan Nowak works here:")
    Filter.check(emp5)
    println("Employing Jan Nowak")
    Filter.add(emp5)
    println("Checking if Jan Nowak works here:")
    Filter.check(emp5)
    println("Checking if Filip Jaszczyk works here:")
    Filter.check(emp1)
    println("Firing Filip Jaszczyk")
    Filter.delete(emp1)
    println("Checking if Filip Jaszczyk works here:")
    Filter.check(emp1)
  }
}
