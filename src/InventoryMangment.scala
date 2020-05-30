
import scala.io.StdIn._

object InventoryMangment {
  //main Method
  def main(args: Array[String]): Unit = {

    // create new object
    val obj = new InventoryManagement
    //variable declaration and default value
    var name:String = ""
    var quantity = 0
    var input:String = ""
    var choice:Int=0
    var index:Int = 0
    var valid = false

    //user prompt to input a value
    println("================================================== ")
    println("Inventory Management System!!")
    //call a method
    initial()

    //some method
    def initial():Unit = {

      //loop
      while (!valid) {

        // user input prompt
        println("-------------------------------------------------- ")
        println("Enter a command (add, list, remove, sort, quit) :")

        // read a user string and convert to lower case
        input = scala.io.StdIn.readLine.toLowerCase()

        var response = input
        // conditional statement
        if (response == "add") {
          // calling a method
          add()
          valid = true
        }
        // comparison for user input value
        else if (response == "list") {

          // comparison for user input if no item in a list
          if(obj.NumOfItems== 0){
            // display for the user
            println("There is no item to print!")
          }
          else
          // display all item list
            obj.PrintAllItems()
        }
        //comparison
        else if (response == "remove") {
          if (obj.NumOfItems == 0) {
            //display wrong prompt
            println("There is no item in the system. removing cannot be performed! ")
          }
          else {

            //call method and display the list before it remove the item
            obj.PrintAllItems()

            //call method
            remove_valid()
          }
        }
        // conditional statement
        else if (response == "sort") {
          // comparison #item in a list
          if (obj.NumOfItems > 1){

            // prompt for user to input
            println("0: Sort by item name.")
            println("1: Sort by item quantity.")

            // call a method
            sort_valid()
          }
          // condition when the list have only one value
          else if(obj.NumOfItems == 1) {

            // display for the user
            println("There is only one item in the system. Sorting cannot be performed! ")
          }
          else {
            // display for user if no item in a list
            println("There is no item in the system. Sorting cannot be performed! ")
          }}
        // comparison
        else if (response == "quit") {
          // exit the program
          System.exit(0)
          valid = true
        }
        else {
          // display invalid input value
          println(s"the entered command ($response) is not valid")

        }
      }
    }
    // method to receive user input item
    def add(): Unit = {
      // display for the user to input
      println("enter new item's name :")
      // read a valid value
      name = scala.io.StdIn.readLine().toLowerCase()
      // call another method
      quan()

      // method
      def quan(): Unit = {
        // validate user input
        try {
          println("enter new Item quantity")
          quantity = scala.io.StdIn.readInt()

        } catch {
          // display wrong input
          case _: NumberFormatException => println("the new Item quantity has been entered incorrectly ")
            // then call again
            quan()
        }
        //call method and passing a valid value for the class
        obj.AddInventoryItem(name,quantity)
        // call method
        initial()
      }

    }
    // method for validate sort
    def sort_valid(): Unit = {

      try {
        // prompt for input
        println("Enter sorting option:")
        //read
        choice = scala.io.StdIn.readInt()
        // comparision
        if (choice == 0) {
          //call a method
          obj.SortByName()
          // conformation prompt
          println("Sort by item name is performed! ")
        }
        // condition for comparison
        else if (choice == 1){

          //call a method
          obj.SortByQuantity()

          // conformation prompt
          println("Sort by item name is performed!  ")
        }
        else {

          // error message prompt
          println("The sort option has been entered incorrectly! ")
          // call method or recursive
          sort_valid()
        }
      } catch {

        // exceptional input valid
        case _: NumberFormatException => println("The sort option has been entered incorrectly! ")

          //recursive or call its owen method
          sort_valid()
      }
    }

    // some method
    def remove_valid(): Unit = {
      try {

        //input prompt
        println("Enter the item number")

        // read integer
        index = scala.io.StdIn.readInt

        //comparision
        if(index >= 1 && index <= obj.NumOfItems) {

          //call and pass a value
          obj.RemoveInventoryItem(index)

          //conformation prompt
          println("The item 1 has been removed successfully! ")
        }
        else {

          // error message prompt
          println("The item number has been entered incorrectly!  ")
          //recursive method
          remove_valid()
        }
      } catch {
        case _: NumberFormatException => println("The item number has been entered incorrectly!  ")

          //recursive method
          remove_valid()
      }
    }

  }
  // class
  class InventoryManagement {
    private var storeInventory: List[InventoryItem] = Nil // inventory storage


    //method for number of items
    def NumOfItems = {

      // size of the store
      storeInventory.size

    }
    //method with two parameter
    def AddInventoryItem(productName: String, productQuantity: Int): Unit = {

      //receive values and store
      storeInventory = storeInventory :+ InventoryItem(productName, productQuantity)
    }

    // method print all items and quantity
    def PrintAllItems():Unit = {

      // print all item with index
      storeInventory.zipWithIndex.foreach{case(x,i) => println((i+1) + ":" + x)}

    }

    //method to remove item
    def RemoveInventoryItem(listPos: Int): Unit = {

      //drop item from list
      storeInventory = storeInventory.take(listPos-1)++ storeInventory.drop(listPos)

    }
    //method for sorting by name
    def SortByName(): Unit = {

      //sort by name
      var s = storeInventory.sortWith(_.itemName < _.itemName)

      //print sorted item
      for((x,i)<- s.zipWithIndex)println((i+1) + ":" + x )
    }

    //method fro sorting by quantity
    def SortByQuantity(): Unit = {

      //sort by quantity
      var q = storeInventory.sortWith(_.itemQuantity < _.itemQuantity)

      //print sorted item
      for((x,i)<- q.zipWithIndex)println((i+1) + ":" + x )
    }
  }
  //case class
  case class InventoryItem(itemName:String,itemQuantity:Int) {

    //Overriding pre-defined toString method
    override def toString: String = s"name: $itemName, quantity: $itemQuantity "
  }


}
