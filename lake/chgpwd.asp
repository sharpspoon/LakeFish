<html>

<head>
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
<meta name="ProgId" content="FrontPage.Editor.Document">
<title>Change your password</title>
</head>

<body background="wallpaper1.jpg">

<!--<form ACTION="http://<%response.write(Application("name"))%>/lake/newpwd.asp" method="POST">-->
<form method="post" action="http://<%response.write(Application("name"))%>:8080/servlet/chgpwd">

  <p><b><font color="#0000FF"><span lang="en-us">Please feel free to change your 
  password, but do remember it.</span></font></b></p>
  <table border="0" cellpadding="0" cellspacing="0" style="border-collapse: collapse" bordercolor="#111111" width="76%" id="AutoNumber1">
    <tr>
      <td width="31%">
      <p align="right"><font color="#000080"><span lang="en-us">User ID:&nbsp;&nbsp;&nbsp;
      </span></font></td>
      <td width="69%"><input type="text" name="userid" size="20"></td>
    </tr>
    <tr>
      <td width="31%">
      <p align="right"><span lang="en-us"><font color="#000080">New Password:
      </font>&nbsp;&nbsp; </span></td>
      <td width="69%"><span lang="en-us">
      <input type="password" name="newpassword" size="20"></span></td>
    </tr>
    <tr>
      <td width="31%">
      <p align="right"><span lang="en-us">&nbsp;<font color="#000080">Confirm 
      New Password: </font>&nbsp;&nbsp; </span></td>
      <td width="69%"><span lang="en-us">
      <input type="password" name="confirmnewpassword" size="20"></span></td>
    </tr>
    <tr>
      <td width="31%">
      <p align="right"><span lang="en-us"><font color="#000080">Old Password:&nbsp;
      </font>&nbsp;</span></td>
      <td width="69%"><span lang="en-us">
      <input type="password" name="oldpassword" size="20"></span></td>
    </tr>
    <tr>
      <td width="31%">&nbsp;</td>
      <td width="69%">&nbsp;</td>
    </tr>
    <tr>
      <td width="31%">
      <p align="right"><input type="submit" value="Submit" name="submit"><span lang="en-us">&nbsp;&nbsp;&nbsp;
      </span></td>
      <td width="69%"><span lang="en-us">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      </span><input type="reset" value="Clear" name="clear"></td>
    </tr>
  </table>
  <p><span lang="en-us">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  </span><span lang="en-us">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  </span></p>
</form>

</body>

</html>