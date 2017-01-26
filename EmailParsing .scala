/*
2.	Write regular expression for email parsing.
Description :
   	val EMAIL = “ regular expression definition”.r
  	val EMAIL(user, domain) = “knol@knoldus.com”
      		user = knol


*/



object EmailParsing  extends App{
 
    val Email ="""([a-zA-Z0-9_+-\.]+)@([a-zA-Z0-9\.]+)""".r
    val Email(user, domain) = "knol@knoldus.com"
    println(s"user=$user")
     println(s"domain=$domain")
 }