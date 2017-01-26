/*
1.	Complete  Extractor definition  for URL parsing.

object URL{

  def apply(protocal:String,domain:String,path:String, params:Map[String,String]):String = {
	 //definition
  }

  def unapply(url:String):Option[(String , String, String,Map[String,String])] = {
    	    //definition
  }

}

*/



object URL
{
  
  def apply(protocal:String,domain:String,path:String, params:Map[String,String]):String =
  {
  
	    val buf = new  StringBuilder("")
	 
	    for((k,v)<-params) 
       {
           buf.append(k+"="+v+"&")// here key and value added to string
       }
	 
	     protocal+"://"+domain+path+"?"+buf.substring(0,(buf.length-1))
   }

  def unapply(x:String):Option[(String , String, String,Map[String,String])] = {
  
      val item = x split "\\?"
      if(item.length==2) 
       {
           val part1 = item(0)
           val part2 = item(1) split "\\&"
     
           if(part2.length==3)
           {
              val part3 = part2(0) split("=")
              val part4 = part2(1) split("=")
              val part5 = part2(2) split("=")
       
               if(part3.length==2 && part4.length==2 && part5.length==2)
                 {
                    val map = Map(part3(0)->part3(1),part4(0)->part4(1),part5(0)->part5(1))
                    val res1= part1 split "\\://"
         
                    if(res1.length==2)
                      {
                          val protocal = res1(0)
                          val res2 = res1(1) split "\\/"
           
                           if(res2.length==3)
                            {
                                val domain1= res2(0)
                                val path = "/"+res2(1)+"/"+res2(2)
                                Some(protocal,domain1,path,map)
                 
                             }else None
                         
                       } else None
                      
                   } else None
                  
             } else None
             
     } else None
  }
}

object UrlTesting extends App {
  
   val map =  Map("state" -> "hash", "isauthcode" -> "true", "code" -> "112")
   val obj = URL("https","aws.amazon.com","/console/home",map)
   println(obj)
  
   obj match 
    {
     
       case URL(protocal,domain,path ,params) => println(s"protocol= $protocal \ndomain=  $domain \npath= $path \nparams= $params")
       case _=> println("not valid url")
    }
 }