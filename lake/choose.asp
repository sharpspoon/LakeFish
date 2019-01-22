<html>

<head>
<title>choosepage</title>
<script type="text/javascript" language="JavaScript">
<!--
function hasSpace(elm) {
   var elmstr = elm.value;
   var flagSpace = 0;
   for (var i = 1; i < elmstr.length; i++) {
      if (elmstr.charAt(i) == " ") flagSpace++;
   }
   if( flagSpace > 0 ) return false;
	 else return true;
}
//-->
</script>
</head>

<body text="#FFFFFF" background="wallpaper1.jpg">

<p><br>
<br>
</p>

<form method="get" action="http://<%response.write(Application("name"))%>:8080/servlet/Getstation2">
  <big><strong><big><big><div align="center"><center><p><font color="#000000">Lake
  Information</font></big></big></strong></big></p>
  </center></div><div align="center"><center><p>
    <img src="bar1.gif" alt="bar1.gif (417 bytes)" width="520" height="9"> </p>
  </center></div><table border="0" width="900" height="2">
    <tr>
      <td width="15%"></td>
      <td width="65%">&nbsp;<p>&nbsp;</p>
      <p><font color="#000000"><strong><big>Simulation Title </big></strong><input type="text" name="simu_title" size="44" value="Descriptive information for your simulation"> </font></p>
      <p>&nbsp;</td>
      <td width="20%"></td>
    </tr>
    <tr>
      <td width="15%"></td>
      <td width="65%"><font color="#000000"><strong><big>Lake Name<big> </big></big></strong><input type="text" name="lakename" size="20" 
                 onchange="var flag=hasSpace(this); if(!flag) {this.value=''; 
                 alert('The Lake Name must be one word!!!!');}"></font><p>&nbsp;</td>
      <td width="20%"></td>
    </tr>
    <tr>
      <td width="15%"></td>
      <td width="65%"><font color="#000000"><strong><big>Select a U.S. State where the lake is</big></strong></font></td>
      <td width="20%"></td>
    </tr>
    <tr>
      <td width="15%"></td>
      <td width="65%"><table border="0" width="600" cellspacing="0" cellpadding="0">
        <tr>
          <td width="27%"></td>
          <td width="42%"><select name="State" size="1">
            <option VALUE="Choose" SELECTED>Select State</option>
            <option value="Choose">---------------------</option>
            <option VALUE="Alabama">Alabama</option>
            <option VALUE="Arizona">Arizona</option>
            <option VALUE="Arkansas">Arkansas</option>
            <option VALUE="California">California</option>
            <option VALUE="Colorado">Colorado</option>
            <option VALUE="Connecticut">Connecticut</option>
            <option VALUE="Delaware">Delaware</option>
            <option VALUE="Florida">Florida</option>
            <option VALUE="Georgia">Georgia</option>
            <option VALUE="Idaho">Idaho</option>
            <option VALUE="Illinois">Illinois</option>
            <option VALUE="Indiana">Indiana</option>
            <option VALUE="Iowa">Iowa</option>
            <option VALUE="Kansas">Kansas</option>
            <option VALUE="Kentucky">Kentucky</option>
            <option VALUE="Louisiana">Louisiana</option>
            <option VALUE="Maine">Maine</option>
            <option VALUE="Maryland">Maryland</option>
            <option VALUE="Massachusetts">Massachusetts</option>
            <option VALUE="Michigan">Michigan</option>
            <option VALUE="Minnesota">Minnesota</option>
            <option VALUE="Mississippi">Mississippi</option>
            <option VALUE="Missouri">Missouri</option>
            <option VALUE="Montana">Montana</option>
            <option VALUE="Nebraska">Nebraska</option>
            <option VALUE="Nevada">Nevada</option>
            <option VALUE="NewHampshire">New Hampshire</option>
            <option VALUE="NewJersey">New Jersey</option>
            <option VALUE="NewMexico">New Mexico</option>
            <option VALUE="NewYork">New York</option>
            <option VALUE="NorthCarolina">North Carolina</option>
            <option VALUE="NorthDakota">North Dakota</option>
            <option VALUE="Ohio">Ohio</option>
            <option VALUE="OKlahoma">Oklahoma</option>
            <option VALUE="ORegon">Oregon</option>
            <option VALUE="Pennsylvania">Pennsylvania</option>
            <option VALUE="RhodeIsland">Rhode Island</option>
            <option VALUE="Southcarolina">South Carolina</option>
            <option VALUE="Southdakota">South Dakota</option>
            <option VALUE="Tennessee">Tennessee</option>
            <option VALUE="Texas">Texas</option>
            <option VALUE="UTah">Utah</option>
            <option VALUE="Vermont">Vermont</option>
            <option VALUE="Virginia">Virginia</option>
            <option VALUE="WAshington">Washington</option>
            <option VALUE="Dc">Washington, D.C.</option>
            <option VALUE="Westvirginia">West Virginia</option>
            <option VALUE="WIsconsin">Wisconsin</option>
            <option VALUE="WYoming">Wyoming</option>
          </select>&nbsp;&nbsp;&nbsp; <input type="submit" name="Submit" value="Submit"></td>
          <td width="31%"></td>
        </tr>
      </table>
      </td>
      <td width="20%"></td>
    </tr>
  </table>
  <div align="center"><center><p>&nbsp;</p>
  </center></div><table border="0" cellpadding="0" width="513">
    <tr>
      <td width="125"></td>
      <td width="260"><strong><big><font color="#000000">weather data information:</font></big></strong></td>
      <td width="86"><a href="Weather-Data.htm"><strong><font color="#000000">Text file</font></strong></a></td>
      <td width="34"><a href="Weather-Data.pdf"><strong><font color="#000000">PDF</font></strong></a></td>
    </tr>
  </table>
  <div align="center"><center><p><br>
  </p>
  </center></div>
</form>

<p align="center">&nbsp;</p>
<div align="center"><center>

<table border="0" width="700" bordercolor="#FFFFFF" bordercolorlight="#FFFFFF" bordercolordark="#FFFFFF">
  <tr>
    <td width="97%"><font face="Courier New" color="#000040">If&nbsp; you want to simulate
    water quality at your lake after 1995, you need provide daily weather data to us<small>&nbsp;</small></font></td>
    <td width="5%"><img src="gobook.gif" alt="gobook.gif (760 bytes)" WIDTH="27" HEIGHT="18"></td>
  </tr>
</table>
</center></div>

<p align="center"><big>&nbsp;&nbsp; </big></p>
<!--webbot bot="HTMLMarkup" TAG="XBOT" StartSpan --></p><!--webbot BOT="HTMLMarkup" endspan -->


<p align="center">&nbsp;</p>

<p><input TYPE="button" VALUE="|&lt;" ONCLICK="main.firstPage();"> <input TYPE="button" VALUE="&lt;" ONCLICK="main.previousPage();"> <input TYPE="button" VALUE="&gt;" ONCLICK="main.nextPage();"> <input TYPE="button" VALUE="&gt;|" ONCLICK="main.lastPage();">
</p>
</body>
</html>