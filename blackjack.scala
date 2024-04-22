// Author: Emily Weinstein
// File: blackjack.scala
// A simple Blackjack game as part of my individual contribution to the term project.
// Source: https://docs.scala-lang.org/ -- the official documentation for the langauge

import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine

/**
 * The main function facillitates the operation of the program
 * 
 * */
object Main extends App {
  println("\nHello! We're gonna play a little game of Blackjack.\n")

  //CITE: Matthew Maillet
  //DESC: The functionality of the sleep function as a timer
  Thread.sleep(2000)
  println("The goal of the game is to have a hand that totals 21 points or to score higher \nthan the dealer. Number cards are worth their value, face cards are all worth \n10, and aces can be either 1 or 11.")
  Thread.sleep(2000)
  println("Hit the enter key to begin :)\n")
  readLine()
  var gameStatus = true
  while (gameStatus) { // Allows the player to play as many times as they want
    beginRound
    println("")
    println("Let's play again! Press \"1\" continue or \"2\" to quit.")
    val response = readLine().toInt
    if (response == 1) { gameStatus = true }
    if (response == 2) { gameStatus = false }
  }
}

/**
 * This function facillitates a single round of gameplay. It creates the deck, player, and card objects.
 * It accounts for all possible outcomes of a game based on input player responses.
 * @param N/A
 * @return N/A
 * */
def beginRound : Unit = { 
  val defaultCards = ArrayBuffer[Card]()
  val newDeck = Deck(defaultCards, 52)
  newDeck.createDeck
  newDeck.shuffle

  // CITE: Sebastian Favela's code from previous submission Project1.scala
  // DESC: How to use the ArrayBuffer object.

  val dealerHand = ArrayBuffer[Card]() // Creates player and dealer hands
  val playerHand = ArrayBuffer[Card]()
  val dealer = Player(dealerHand)
  val player = Player(playerHand)
  dealer.addToHand(newDeck.deal)
  dealer.addToHand(newDeck.deal)
  player.addToHand(newDeck.deal)
  player.addToHand(newDeck.deal)
  
  var valid = true // Bool vars for outcome of game
  var bust = false
  var fold = false

  dealer.printDealerHand
  player.printHand("Your hand: ")
  if (player.hit21) { valid = false }
  while (valid) {
    println("Do you want another card? \"1\" for yes, \"2\" for no, and \"3\" to fold.")
    val response = readLine().toInt

    if (response == 1) { // If player hits
      player.addToHand(newDeck.deal)
      valid = player.validHand
      player.printHand("Your hand: ")
    } else if (response == 2) { // If player stays
      valid = false
    } else if (response == 3) { // If player folds
      valid = false
      fold = true
    } else {
      println("Invalid response, try again.")
    }

    if (player.validHand == false) { // If player busts
      valid = false
      bust = true
    } else if (player.hit21) { // If hand has total of 21
      valid = false
    }
  }
  if (bust) { 
    println("Bust! You've lost.")
  } else if (fold) { println("You've folded! Better luck next time.")
  } else if (player.hit21) { println("Blackjack! You've won the game, congrats!")
  } else { 
    hitDealer(dealer, newDeck)
    if (! dealer.validHand){ println("Bust! You win! Congrats!") }
    else if (dealer.getHandValue > player.getHandValue) { println("The dealer won!") }
    else if (dealer.getHandValue == player.getHandValue) { println("It's a tie! No winners.") }
    else { println("You won! You are truly the best Blackjack player.") }
  }
}

/**
 * At the end of the round if the player did not bust or get Blackjack, we add cards to dealer's hand
 * @param dealer, a dealer object with a hand of cards
 * @param deck, the shuffled deck of cards so we may draw
 * @return N/A
 * */
def hitDealer(dealer: Player, deck: Deck): Unit = {
  println("Ok then, let's check the dealer.")
  dealer.printHand("The dealer's hand is:")
  var didDraw = false
  while (dealer.getHandValue < 17) // Dealer must hit if their hand sums to under 17
    dealer.addToHand(deck.deal)
    didDraw = true
  if (didDraw) { dealer.printHand("The dealer's final hand is:") }
}

/**
 * The Player class with methods essential to a player in the Blackjack game
 * @param hand, the array of Card objects that make up the player's hand.
 * */
class Player(var hand: ArrayBuffer[Card]):
  /**
   * Adds a card to the player's hand
   * @param card, a card to add to the hand
   * */
  def addToHand(card: Card): Unit = 
    hand += card
  /**
   * Prints the player's hand out
   * @param message, the message to print before the hand is printed
   * */
  def printHand(message: String): Unit = // Prints hand
    println(message)
    for (card <- hand) {
      card.printCard
    }
    println("Current score:")
    println(getHandValue)
    println("")
  /**
   * A method specific to the dealer player, hides the first card.
   * */
  def printDealerHand: Unit = 
    println("The dealer has one card face down. The other is:")
    hand(0).printCard
    println("")
  /**
   * Totals the hand value, taking aces into account
   * @return the integer value
   * */
  def getHandValue: Int = 
    var score = 0
    var ace = false
    for (card <- hand) {
      if (card.getValue == 1) {
        ace = true
        if ((score + 11) <= 21) { score += 11}
        else { score += card.getValue }
      }
      else { score += card.getValue }
    }
    if (ace && (score > 21))
      score -= 10
    score
  /**
   * Checks if player busts
   * @return false if player lost, true otherwise
   * */
  def validHand: Boolean = // Checks if player busts
    if (getHandValue > 21) { false } else { true }
  /**
   * Checks if player has 21 points
   * @return true if so, false otherwise
   * */
  def hit21: Boolean = // Checks for blackjack
    if (getHandValue == 21) { true } else { false }
end Player

/**
   * A deck object for the cards in the game
   * @param deckOrder, the array of Card objects in deck
   * @param size, the number of cards in the deck
   * */
class Deck(var deckOrder: ArrayBuffer[Card], var size: Int):
  /**
   * Creates a new Deck object, creating new cards to populate the deck
   * */
  def createDeck: Unit = 
    deckOrder = ArrayBuffer[Card]()
    val VALUES = Vector(1,2,3,4,5,6,7,8,9,10,11,12,13)
    val SUITS = Vector("Hearts", "Spades", "Diamonds", "Clubs")
    for (vals <- VALUES) {
      for (suit <- SUITS) {
        val card = Card(vals, suit)
        deckOrder += card
      }
    }
  /**
   * Prints the entire deck out, used for troubleshooting
   * */
  def printDeck: Unit = 
    for (card <- deckOrder) {
      card.printCard
    }
  /**
   * Randomly shuffles the deck
   * */
  def shuffle: Unit =  
    val deck = ArrayBuffer[Card]()
    for (card <- deckOrder) {
      deck += Card(0,"")
    }
    val rand = new scala.util.Random
    for (index <- 0 to 51) {
      var nIndex = rand.nextInt(52)
      while (deck(nIndex).getValue != 0) {
         nIndex = rand.nextInt(52)
      }
      deck(nIndex) = deckOrder(index)
    }
    deckOrder = deck
  /**
   * Draws a single card out of the deck
   * @return the top card
   * */
  def deal: Card =
    size -= 1
    deckOrder(size)
end Deck

/**
   * The Card class with individual card objects
   * @param value, the number value of the Card
   * @param suit, the suit of the card
   * */
class Card(var value: Int, var suit: String): // Card class
    /**
     * Prints the value and suit of the card
     * */
    def printCard: Unit = // prints a single card value
      val faceValue = getFaceName
      println(s"$faceValue of $suit")
    /**
     * Gets the name of each card, accounting for face cards and aces
     * @return the string form of the value
     * */
    def getFaceName: String = // For printing the card
      if (value == 11) {
        "J"
      } else if (value == 12) {
        "Q"
      } else if (value == 13) {
        "K"
      } else if (value == 1) {
        "A"
      } else {
        value.toString
      }
    /**
     * Gets the value of each card
     * @return the integer value of the card
     * */
    def getValue: Int = // Gets value of a card 
      if (value == 11 | value == 12 | value == 13) {
        10
      } else {
        value
      }
end Card