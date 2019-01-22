<html>

<head>
<title>Re</title>
</head>

<body background="wallpaper1.jpg">

<h2 align="center">Re-run options</h2>

<table border="0" width="90%" bgcolor="#008000" height="1" bordercolor="#FFFFFF"
bordercolorlight="#FFFFFF" bordercolordark="#008000">
  <tr>
    <td width="19%" height="0.5"></td>
    <td width="600" height="0.5" bordercolor="#FFFFFF" bordercolorlight="#FFFFFF"></td>
    <td width="44%" height="0.5"></td>
  </tr>
</table>

<p>&nbsp;</p>

<form mothod="get" action="http://<%response.write(Application("name"))%>:8080/servlet/Rerun_modify">
  <div align="center"><center><table border="0" cellpadding="0" width="677" cellspacing="0">
    <tr>
      <td width="414" bgcolor="#C0C0C0"><strong><big>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 
      Review results :</big></strong></td>
      <td width="247" bgcolor="#C0C0C0"><input type="radio" value="view" checked name="R0"> <big>yes</big></td>
    </tr>
    <tr>
      <td width="414" bgcolor="#FFFFFF"><strong><big>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      Review results<span lang="en-us"> (Future)</span> :</big></strong></td>
      <td width="247" bgcolor="#FFFFFF"><input type="radio" value="future" name="R0"> <big>yes</big></td>
    </tr>
    <tr>
      <td width="414" bgcolor="#C0C0C0"><strong><big>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      Predict fish habitat :</big></strong></td>
      <td width="247" bgcolor="#C0C0C0"><input type="radio" value="habitat" name="R0"> <big>yes</big></td>
    </tr>
    <tr>
      <td width="306" bgcolor="#FFFFFF"><strong><big>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      Weather station:</big></strong></td>
      <td width="234" bgcolor="#FFFFFF"><input type="radio" value="station" name="R0"> <big>change</big></td>
    </tr>
    <tr>
      <td width="306" bgcolor="#C0C0C0"><strong><big>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      Lake geometry:</big></strong></td>
      <td width="234" bgcolor="#C0C0C0"><input type="radio" value="geometry" name="R0"> <big>change</big></td>
    </tr>
    <tr>
      <td width="306" bgcolor="#FFFFFF"><strong><big>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      Model parameter:</big></strong></td>
      <td width="234" bgcolor="#FFFFFF"><input type="radio" value="para" name="R0"> <big>change</big></td>
    </tr>
    <tr>
      <td width="306" bgcolor="#C0C0C0"><strong><big>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      
      Lake Parameter Files</big></strong></td>
      <td width="234" bgcolor="#C0C0C0"><input type="radio" value="download" name="R0"> <big>download</big></td>
    </tr>
  </table>
  </center></div><div align="center"><center><p><input type="submit" name="submit"
  value="continue"></p>
  </center></div>
</form>

<p><input TYPE="button" VALUE="|&lt;" ONCLICK="contents.firstPage();"> <input
TYPE="button" VALUE="&lt;" ONCLICK="contents.previousPage();"> <input TYPE="button"
VALUE="&gt;" ONCLICK="contents.nextPage();"> <input TYPE="button" VALUE="&gt;|"
ONCLICK="contents.lastPage();"> </p>
</body>
</html>