package fdm

import javax.xml.transform.dom.DOMSource

/**
 * Sometimes we don't want to take the time to model data precisely. For example, we might want to
 * model an email address with a string, even though most strings are not valid email addresses.
 *
 * In such cases, we can save time by using a smart constructor, which lets us ensure we model
 * only valid data, but without complicated data types.
 */
object smart_constructors {
  sealed abstract case class Email private (value: String)
  object Email {
    def fromString(email: String): Option[Email] =
      if (email.matches("""/\w+@\w+.com""")) Some(new Email(email) {}) else None
  }

  /**
   * EXERCISE 1
   *
   * Create a smart constructor for `NonNegative` which ensures the integer is always non-negative.
   */
  sealed abstract case class NonNegative private (value: Int)
  object NonNegative {
    def fromInt(n: Int): Option[NonNegative] =
      if (n >= 0) Some(new NonNegative(n) {}) else None
  }

  /**
   * EXERCISE 2
   *
   * Create a smart constructor for `Age` that ensures the integer is between 0 and 120.
   */
  sealed abstract case class Age private (value: Int)
  object Age {
    def fromInt(age: Int): Option[Age] =
      if (age >= 0 && age <= 120) Some(new Age(age) {}) else None
  }

  /**
   * EXERCISE 3
   *
   * Create a smart constructor for password that ensures some security considerations are met.
   */
  sealed abstract case class Password private (value: String)
  object Password {
    def fromString(s: String): Option[Password] =
      // taken from https://bit.ly/3brEYqh
      if (s.matches("^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[@$!%*?&])[A-Za-z\\d@$!%*?&]{8,10}$"))
        Some(new Password(s) {})
      else None
  }

  sealed abstract case class NonEmptyString private (value: String)
  object NonEmptyString {
    def apply(s: String): Option[NonEmptyString] =
      if (s.nonEmpty) Some(new NonEmptyString(s) {}) else None
  }

  sealed abstract case class Money private (fractionalUnits: NonNegative, currency: NonEmptyString)
  object Money {
    def apply(fractionalUnits: Int, currency: String): Option[Money] =
      for {
        fractionalUnits <- NonNegative.fromInt(fractionalUnits)
        currency        <- NonEmptyString(currency)
      } yield new Money(fractionalUnits, currency) {}
  }

  sealed abstract case class NonFutureInstant private (instant: java.time.Instant)
  object NonFutureInstant {
    def apply(instant: java.time.Instant): Option[NonFutureInstant] = {
      val now = java.time.Instant.now()
      if (instant.isBefore(now) || instant == now) Some(new NonFutureInstant(instant) {})
      else None
    }
  }
}

object applied_smart_constructors {
  import smart_constructors._

  /**
   * EXERCISE 1
   *
   * Identify the weaknesses in this data type, and use smart constructors (and possibly other
   * techniques) to correct them.
   */
  sealed abstract case class BankAccount private (
    id: NonEmptyString,
    name: NonEmptyString,
    balance: Money,
    opened: NonFutureInstant
  )
  object BankAccount {
    def apply(id: String, name: String, balance: Int, currency: String, opened: java.time.Instant) =
      for {
        id      <- NonEmptyString(id)
        name    <- NonEmptyString(name)
        balance <- Money(balance, currency)
        opened  <- NonFutureInstant(opened)
      } yield new BankAccount(id, name, balance, opened) {}
  }

  /**
   * EXERCISE 2
   *
   * Identify the weaknesses in this data type, and use smart constructors (and possibly other
   * techniques) to correct them.
   */
  sealed abstract case class Person private (age: NonNegative, name: NonEmptyString, salary: Money)
  object Person {
    def apply(age: Int, name: String, salaryFractionalUnits: Int, currency: String) =
      for {
        age    <- NonNegative.fromInt(age)
        name   <- NonEmptyString(name)
        salary <- Money(salaryFractionalUnits, currency)
      } yield new Person(age, name, salary) {}
  }

  /**
   * EXERCISE 3
   *
   * Identify the weaknesses in this data type, and use smart constructors (and possibly other
   * techniques) to correct them.
   */
  sealed abstract class EventType(value: Int)
  object EventType {
    case object PortScanning    extends EventType(0)
    case object DenialOfService extends EventType(1)
    case object InvalidLogin    extends EventType(2)

    def apply(code: Int) = code match {
      case 0 => Some(PortScanning)
      case 1 => Some(DenialOfService)
      case 2 => Some(InvalidLogin)
      case _ => None
    }
  }

  sealed abstract case class SecurityEvent private (
    machine: NonEmptyString,
    timestamp: NonFutureInstant,
    eventType: EventType
  )
  object SecurityEvent {
    def apply(machine: String, timestamp: java.time.Instant, eventType: Int) =
      for {
        machine   <- NonEmptyString(machine)
        timestamp <- NonFutureInstant(timestamp)
        eventType <- EventType(eventType)
      } yield new SecurityEvent(machine, timestamp, eventType) {}
  }
}
