import Email.*

object Email:
  protected trait NotValidated
  protected trait Validated

  protected case class EmailAddress[A](address: String)

  val mkEmail: String => EmailAddress[NotValidated] = EmailAddress.apply

  def validateAddress(
      e: EmailAddress[NotValidated]
  ): Option[EmailAddress[Validated]] =
    if e.address matches "([0-9a-zA-Z- ]+)@([0-9a-zA-Z- ]+)\\.com"
    then Some(EmailAddress[Validated](e.address))
    else None

  def sendEmail(email: EmailAddress[Validated]): Unit = println("Email sent!")

val notValidatedEmail = mkEmail("geza@example.com")

val validatedEmail = validateAddress(notValidatedEmail).get // Don't do this!

sendEmail(validatedEmail)
// sendEmail(notValidatedEmail)

// EmailAddress[Validated]("nemvalid")

// ------------------------

import cats.*
import cats.implicits.*
import cats.data.Validated as V

object EmailWithValidated:
  protected trait NotValidated
  protected trait Validated

  enum ValidationError:
    case InvalidHost
    case InvalidAddress
    case InvalidDomain
  import ValidationError.*

  protected case class EmailAddress[A](address: String)

  val mkEmail: String => EmailAddress[NotValidated] = EmailAddress.apply

  def validateAddress(
      e: EmailAddress[NotValidated]
  ): V[List[ValidationError], EmailAddress[Validated]] =
    if e.address matches "([0-9a-zA-Z- ]+)@.*"
    then EmailAddress[Validated](e.address).valid
    else List(InvalidAddress).invalid

  def validateHostname(
      e: EmailAddress[NotValidated]
  ): V[List[ValidationError], EmailAddress[Validated]] =
    if e.address matches "(.*@[0-9a-zA-Z- ]+)\\..*"
    then EmailAddress[Validated](e.address).valid
    else List(InvalidHost).invalid

  def validateDomain(
      e: EmailAddress[NotValidated]
  ): V[List[ValidationError], EmailAddress[Validated]] =
    if e.address matches ".*@.*\\.com"
    then EmailAddress[Validated](e.address).valid
    else List(InvalidDomain).invalid

  def validateEmail(
      e: EmailAddress[NotValidated]
  ): V[List[ValidationError], EmailAddress[Validated]] =
    validateAddress(e) *> validateHostname(e) *> validateDomain(e)


  def sendEmail(email: EmailAddress[Validated]): Unit = println("Email sent!")

import EmailWithValidated.*

val e = EmailWithValidated.mkEmail("ge&za@exa*mple.c&om")

validateEmail(e)

val plus: Int => Int => Int = n => m => n + m
Option(plus) *> Option(2) <* Option(3)
