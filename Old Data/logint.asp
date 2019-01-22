<html>

<head>
<title>login</title>
</head>

<body background="wallpaper1.jpg">

<p>&nbsp;</p>

<p align="center"><big><big><font face="Arial" color="#400040">Water Quality and Fish
Habitat Simulation System - MINILAKE96 </font></big></big></p>

<hr width="90%" color="#000000">
<form method="get" action="http://<%response.write(Application("name"))%>/lake/signup.asp">
<!--<form method="post" action="http://<%response.write(Application("name"))%>:8080/servlet/zhoutestServlet">-->
  <div align="center"><center><h3><marquee border="0"
  style="color: rgb(255,0,0); font-size: large" direction="right" scrolldelay="75"
  scrollamount="3">Welcome to using the simulation system</marquee></h3>
  </center></div><div align="center"><center><table border="0" cellpadding="0" width="813"
  cellspacing="0">
    <tr>
      <td width="813"><strong><big><font color=blue>I am a new user, Please sign me up.</font><big>&nbsp; </big></big></strong><font color="#800040"><input type="submit" value="new user" name="newuser" </center></font></td>
    </tr>
  </table>
  </center></div><!--<div align="center"><center><p>&nbsp;<font color="#800040"></p>
  </center></div>-->
</form>

<form method="post" action="http://<%response.write(Application("name"))%>:8080/servlet/Logint">

  <div align="center"><center><p>&nbsp;</p>
  </center></div></font><div align="center"><center><table border="0" cellpadding="0"
  width="820" cellspacing="0">
    <tr>
      <td width="820"><strong><font color="#000000"><big><font color=blue>If you have a user ID, you can login the system:</font></big>
Enter model input data;Access/Edit input data entered previously;Run the model;View previous simulation results.
</big></strong><p><strong><font color="#000000"><big>&nbsp;</font></strong></td>
    </tr>
  </table>
  </center></div><font color="#800040"><div align="center"><center><table border="0"
  cellpadding="0" width="300">
    <tr>
      <td width="224"><big><big><font color="#000000">userID </font></big></big>
<input type="text" name="uid" size="12"></td>


      
    </tr>

  <tr>
      <td width="224"><big><font color="#000000">password </font></big>
<input type="password" name="passwd" size="12"></td>


      <td width="54"><input type="submit" value="login" name="login"></td>
    </tr>

  </table>
  </center></div>
  <input type="hidden" name="hostname" value="<%response.write(Application("name"))%>">
</form>
</font>
<form method="POST" action="logint.asp" webbot-action="--WEBBOT-SELF--">
  <!--webbot bot="SaveResults" u-file="_private/form_results.csv" s-format="TEXT/CSV" s-label-fields="TRUE" startspan --><input TYPE="hidden" NAME="VTI-GROUP" VALUE="0"><!--webbot bot="SaveResults" endspan i-checksum="43374" --><p>&nbsp;</p>
  <p>&nbsp;</p>
</form>
<form method="POST" action="http://<%response.write(Application("name"))%>/lake/chgpwd.asp">
  <span lang="en-us"><b><font color="#0000FF" size="4">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Change Password? Click</font></b>
  </span><input type="submit" value="Change Password" name="chgpwd"><span lang="en-us">.</span></p>
</form>
</body>
</html>