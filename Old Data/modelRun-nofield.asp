<!-- ModelRun.html : set a request to server (lakefish) and start running Minilake Model -->
<html>

<head>
<title>Running MiniLake Model</title>
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
</head>

<body text="#000000" background="wallpaper1.jpg">

<h2 align="center">Simulation Options</h2>

<table border="0" width="90%" bgcolor="#008000" height="1" bordercolor="#FFFFFF"
bordercolorlight="#FFFFFF" bordercolordark="#008000">
  <tr>
    <td width="19%" height="0.5"></td>
    <td width="600" height="0.5" bordercolor="#FFFFFF" bordercolorlight="#FFFFFF"></td>
    <td width="44%" height="0.5"></td>
  </tr>
</table>

<p>&nbsp;</p>
<form METHOD="POST" ACTION="http://<%response.write(Application("name"))%>:8080/servlet/MinilakeSimulation">

  <div align="center"><center><table border="0" cellpadding="0" width="677" cellspacing="0">
    <tr>
      <td width="372" bgcolor="#FFFFFF"><strong><big>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      Start Simulation:</big></strong></td>
      <td width="168" bgcolor="#FFFFFF"><input type="radio" value="simulate" checked name="R0"> <big>yes</big></td>
    </tr>
    <tr>
      <td width="372" bgcolor="#C0C0C0"><strong><big>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      Start Simulation<span lang="en-us"> (Future)</span>:</big></strong></td>
      <td width="168" bgcolor="#C0C0C0"><input type="radio" value="future" name="R0"> <big>yes</big></td>
    </tr>
    <tr>
      <td width="372" bgcolor="#FFFFFF"><strong><big>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
      
      Lake Parameter Files</big></strong></td>
      <td width="168" bgcolor="#FFFFFF"><input type="radio" value="download" name="R0"> <big>download</big></td>
    </tr>
  </table>
  </center></div><div align="center"><center><p><input type="submit" name="sumbit"
  value="continue"></p>
  </center></div>
</form>
</body>
</html>