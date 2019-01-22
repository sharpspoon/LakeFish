<!-- ModelRun.html : set a request to server (lakefish) and start running Minilake Model -->
<html>

<head>
<title>Running MiniLake Model</title>
<meta name="GENERATOR" content="Microsoft FrontPage 5.0">
</head>

<body text="#000000" background="wallpaper1.jpg">
<FORM name=ModelRun action=http://<%response.write(Application("name"))%>:8080/servlet/ModelRun 
method=post>
<p>&nbsp;</p>

<p align="center"><big><big><strong>Setup Control Parameters for Your Simulation! </strong></big></big></p>

<DIV 
align=center><INPUT type=submit value="Click to Input" name=modelrun></DIV>
</form><br>
</body>
</html>