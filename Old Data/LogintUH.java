
/**
 *     this servelt program uses to manage user login
 *     Zhou, Jianhua@7/6/2001
*/

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;
import java.sql.*;

public class LogintUH extends HttpServlet
{
    private Connection con=null;

    private int count=0, find=0, runtimes=0;
	private SQLException ee1;
	private ClassNotFoundException cnf1;
    //userid header
    private final String userid_header="Lu20";
    private boolean check=true;
	private String pw;
    //set up the connection
    public void init(ServletConfig config) throws ServletException{
        super.init(config);
        //try to load the initial user ID from the file
        try{
            FileReader fileReader=new FileReader("USERID.INT");
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            String id=bufferedReader.readLine();
            count=Integer.parseInt(id);
        }catch(FileNotFoundException fnf){System.err.println(fnf);}
         catch(IOException ie){ System.err.println(ie); }
         catch(NumberFormatException nfe){System.err.println(nfe); }

        //jdbc-odbc connection
        try{
            //load ODBC driver
            Class.forName("sun.jdbc.odbc.JdbcOdbcDriver");

            con=DriverManager.getConnection("jdbc:odbc:weather","","");
            //out.println(con);

       }catch(SQLException ee){System.err.println("wrong with odbc"+ee);ee1=ee;}
       catch(ClassNotFoundException cnf){ System.err.println(cnf);cnf1=cnf;}
    }

    //handle new user
    public void doPost(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException{
        res.setContentType("text/html");
        PrintWriter out=res.getWriter();
         HttpSession session=req.getSession(true);

        String userid=userid_header+count;
        session.putValue("userid", userid);
        session.putValue("run_times", new Integer(1));
        session.putValue("con", con);
        printPageHeader(out);

         //save new uid into db
        save_uid(userid, out);

        //if save successfully, print uid to user and increase the uid by five each time
        if(check){
            out.println("<center><h3>Please write down your user ID: "+userid+" !!!</h3></center>");
            out.println("<center><h3>Otherwise, you can not access your previous results</h3></center>");
            out.println("<center><h3>Next time use this ID to login</h3></center>");
            out.println("<form action=\"http://cleveland1.cive.uh.edu:8080/servlet/NewUserLogin\" method=\"get\">");
            out.println("<center><input type=submit name=\"sumbit\" value=\"Login\"></center>");
            printPageFooter(out);
            try{
                out.flush();
                out.close();
            }catch(Exception ee){ System.err.println(ee); }

            //synchronize this code block
            synchronized(this){
               count=count+5;
               if(count%50==0)
                  save_count();
            }
        }

    }



    //get info. from client
    public synchronized void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException
    {
        res.setContentType("text/html");
        PrintWriter out=res.getWriter();
        if(ee1!=null){
        	out.println(ee1);
        }
        if(cnf1!=null){
        	out.println(cnf1);
        }
        
        HttpSession session=req.getSession(true);
        String uid=null;
        int flag=0;
        find=0;

        //get the para. from the html if they are non-null
	String _p=null;
        if ((_p = req.getParameter("uid")) != null) {
            uid=_p;
        }
        ////////////**********Insert some code here/////////////
    //////////********************Get the password********************//    
    String _p1=null;
        if ((_p1 = req.getParameter("password")) != null) {
            pw=_p1;
        }

/////////*********************************Insert finished*******************//
        printPageHeader(out);
		//out.println("test");
        //if input is invalid return warning to user
        if(uid.equals("")){flag=1; out.println("<center><h3>Input your user ID to Login</h3></center><BR>");}
        if(flag==1){
            out.println("</body>");
	          out.println("</html>");
	          out.flush();
            try{
                out.close();
            }catch(Exception eer){ System.err.println(eer);}
            return;
        }

        //validate the user ID
        if(flag==0){

            //invoke a query to database and return the results ......
     //       check_uid(uid,out);
			check_uid(uid,pw,out);
			//out.println("reaches here at uid pwd");
            //set a new class to display the previous user-run info.
            if(find==1){
            // 	out.println(uid + " " + pw);
                session.putValue("userid", uid);
                check_userinfo(out, uid);
            }

        }

   }

   //save new uid into db
   private void save_uid(String uid,PrintWriter out){
       try{

            // create and execute the query
	    Statement stmt = con.createStatement();
            String query="INSERT INTO user(user, run_times, lake_name)"+"VALUES('"+uid+"', 1, '*')";
            stmt.executeUpdate(query);
        }catch(SQLException ee){
            check=false;
            System.err.println("wrong with odbc "+ee);
            out.println("Database connecting failure; Try later");
        }
   }
      //*************************Modify here****************************//
     //check the uid ......
     private void check_uid(String ss,String pw,PrintWriter out){

         try{
               //********************************//
            // create and execute the query
	    Statement stmt = con.createStatement();
	    //out.println("create stmt");
            
            String query1="SELECT email FROM test1 WHERE email= '"+ss+"'";
            String query2="SELECT password1 FROM test1 WHERE passwd1='"+pw+"'";
            ResultSet rs1 = stmt.executeQuery(query1);
            ResultSet rs2 = stmt.executeQuery(query2);
	    //check id
	    	//out.println("well reaches here");
            login_me(rs1,rs2, out);

        }catch(SQLException ee){
            System.err.println("wrong with odbc "+ee);
            out.println("<center><h3>Database connecting failure; Try later</h3></center><BR>");
            out.println("</body></html>");
            return;
        }
         catch(Exception ec){ System.err.println("IO Exception " + ec); out.println("IOException: " + ec);}
    }

    /*
     * login
     */
    private void login_me(ResultSet rs1,ResultSet rs2, PrintWriter out)
       throws SQLException
    {
    	while ((rs1.next())&&(rs2.next())) {
       		//out.println("reaches here");
            find=1;
            break;
      }
      if(find==0){
          out.println("<center><h3>Sorry!! your uid or password is invalid. You can not log in</h3></center>");
          out.println("<center><h3>Go back to check your uid and password!!</h3></center>");
          out.println("</body></html>");
          return;
      }
   }
        //******************************************modify finished****************//
   //check the user previous run information
   private void check_userinfo(PrintWriter out, String uid){
      try{

            // create and execute the query
	    Statement stmt = con.createStatement();
            String query="SELECT lake_name FROM user WHERE user= '"+uid+"'";

            ResultSet rs = stmt.executeQuery(query);

            String names=null;
            out.println("<center><h3><strong>Select one of the lakes</strong></h3></center>");
            out.println("<hr>");
            out.println("<center><h3>The following are lakes you have modeled</h3></center>");
            out.println("<br>");
            out.println("<br>");
            out.println("<form action=\"http://cleveland1.cive.uh.edu:8080/servlet/ReLogin\" method=\"get\">");
            out.println("<center><td><select name=\"name\" size=\"1\">");
            out.println("<option VALUE=\"Choose\" SELECTED>Select lake</option>");
            out.println("<option VALUE=\"Choose\">-------------</option>");

            while(rs.next()){
                names=rs.getString("lake_name");
                out.println("<option VALUE=\""+names+"\">"+names+"</option>");
            }
            out.println("<option VALUE=\"new-lake\">new lake</option>");
            out.println("</select>&nbsp; &nbsp; &nbsp;<input type=submit name=\"submit\" value=\"continue\"></center>");
            out.println("</form>");
            printPageFooter(out);

      }catch(SQLException ee){
            System.err.println("wrong with odbc "+ee);
            out.println("<center><h3>Database connecting failure; Try later</h3></center><BR>");
            out.println("</body></html>");
            return;
      }catch(Exception ec){ System.err.println("IO Exception " + ec); }

      if(find==1){   //valid uid
          if(runtimes==1){
              out.println("<center><h3>You have used this Water Quality and Fish Habitat Simulation System</h3></center>");
              out.println("<center><h3>to run one </h3></center>");
          }
      }

   }

   public void destroy() {
   		try{
   		  save_count();
    	  if(con!=null){
     	     con.close();
   	 	  }
   		}catch(SQLException exp){
   	 		System.out.println(exp);
    		exp.printStackTrace();
    	}
    

   }

   //save the userid to the file
    public void save_count(){
      try{
        PrintWriter filewriter=new PrintWriter(new FileWriter("USERID.INT"));
        filewriter.println(count);
        filewriter.flush();
        filewriter.close();
      }catch(Exception e){ System.err.println(e); }
    }

     //get the header of the html which responds user's filling form
     private void printPageHeader(PrintWriter out) {
	      out.println("<html>");
	      out.println("<head>");
	      out.println("<title>login handler</title>");
	      out.println("</head>");
              out.println("<body text=\"#000000\" background=\"http://cleveland1.cive.uh.edu/images/lake/wallpaper1.jpg\">");
	      out.println("<center><font size=5><b>LOGIN INFORMATION</b></font></center>");
    }

    private void printPageFooter(PrintWriter out) {
        out.println("<br>");
        out.println("</body>");
	out.println("</html>");
        out.flush();
        try{
            out.close();
        }catch(Exception eer){ System.err.println(eer); }
    }

}
