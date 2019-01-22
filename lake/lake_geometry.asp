<html>

<head>
<title>geometry</title>
</head>

<body text="#FFFFFF" background="wallpaper1.jpg">

<p><br>
<br>
</p>

<form method="post" action="http://<%response.write(Application("name"))%>:8080/servlet/Geometry">
  <p><strong><big><big><big><font color="#000000">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  Lake Geometry<span lang="en-us"> &amp; Elevation</span></font></big></big></big></strong></p>
  <table border="0" width="90%" bgcolor="#008000" height="1" bordercolor="#FFFFFF"
  bordercolorlight="#FFFFFF" bordercolordark="#008000">
    <tr>
      <td width="19%" height="0.5"></td>
      <td width="37%" height="0.5" bordercolor="#FFFFFF" bordercolorlight="#FFFFFF"></td>
      <td width="44%" height="0.5"></td>
    </tr>
  </table>
  <p><font color="#000000">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; </font></p>
  <table border="0" width="800" bordercolor="#008000" bordercolorlight="#008000"
  bordercolordark="#008000">
    <tr>
      <td width="78"></td>
      <td width="857"><font color="#000000"><strong><big>Surface area</big></strong><small>
      &nbsp; <input type="text" name="area" size="12"><small> <input type="radio" value="km"
      checked name="R1"></small></small><strong>km<sup>2</sup></strong><small> <input
      type="radio" value="mile" name="R1"> </small><strong>mile<sup>2 </sup></strong><small><input
      type="radio" value="acre" name="R1"></small><strong>acre</strong><small><input
      type="radio" value="ft" name="R1"></small><strong>ft<sup>2 </sup></strong><small><input
      type="radio" value="hectare" name="R1"> </small><strong>hectare</strong></font><p>&nbsp;</td>
      <td width="53"></td>
    </tr>
    <tr>
      <td width="78"></td>
      <td width="857"><font color="#000000"><strong><big>Maximum depth </big></strong><small><input
      type="text" name="depth" size="12">&nbsp;&nbsp;&nbsp; <input type="radio" value="m"
      checked name="R2"></small><strong>m </strong><small><input type="radio" value="ft"
      name="R2"></small><strong>ft</strong></font><p>&nbsp;</td>
      <td width="53"></td>
    </tr>
    <tr>
      <td width="78"></td>
      <td width="857"><font color="#000000"><strong><big>Elevation above the mean sea
      level&nbsp;</big></strong><small><input type="text" name="ele" size="14">&nbsp; <small><input
      type="radio" value="m" checked name="R3"></small></small><strong>m </strong><small><small><input
      type="radio" value="ft" name="R3"></small></small><strong>ft
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; </strong><br>
      <br>
      &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;
      &nbsp; &nbsp; &nbsp; &nbsp;<input type="submit" value="Submit" name="B1"><strong>
      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; </strong></font><p>&nbsp;</p>
      <p>&nbsp;</td>
      <td width="53"></td>
    </tr>
    <tr>
      <td width="78"></td>
      <td width="857"></td>
      <td width="53"></td>
    </tr>
  </table>
</form>

<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; </p>

<p><big><strong><big><font face="Courier New">&nbsp;&nbsp;&nbsp;&nbsp; </font></big></strong></big></p>

<p>&nbsp;</p>

<p>&nbsp;</p>

<p><input TYPE="button" VALUE="|&lt;" ONCLICK="main.firstPage();"> <input TYPE="button"
VALUE="&lt;" ONCLICK="main.previousPage();"> <input TYPE="button" VALUE="&gt;"
ONCLICK="main.nextPage();"> <input TYPE="button" VALUE="&gt;|" ONCLICK="main.lastPage();">
</p>
</body>
</html>