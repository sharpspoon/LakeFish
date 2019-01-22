
<html>
<head>
<title>Lake Fish Registration</title>

</head>
<body bgcolor="#fffff0" topmargin=0>
<center>
<FORM ACTION="http://<%response.write(Application("name"))%>/login.asp"
METHOD=POST>

<table border=0 cellpadding=3 cellspacing=4 width="690">

<tr>
  <td colspan=4 bgcolor="#00e0c0">
<font size=7 color="ff0000">LakeFish Registration</font></td>
</tr>


<tr>
	<td colspan=2 bgcolor="#eeeecc">
	<font class="f" size=2 color="#333333"><b><a name="profiletop">User Information</a></b></font>
	</td>
</tr>
				<tr>
			<td >
			<font class="sbd">
				First Name
				</font>
				</td>
				<td>
				<input type="text" name="name" value="" size=23 maxlength=40 value="">
			
                             <td><strong>(Required)</strong></td>
		</tr>
		<tr>
			<td >
			<font class="sbd">
				Last Name
				</font>
				</td>
				<td>
				<input type="text" name="Lname" value="" size=23 maxlength=40 value="">
			</td>
                        <td>(Option)</td>
		</tr>
		
		
			
				
		<tr>
			<td >
			<font class="sbd">Occupation</font>
			</td>
			<td colspan=1>
			<input type="text" name=Occupat value="" size="23"></td>
                       <td>(Option)</td>

</tr>


                     <tr>
			<td >
			<font class="sbd">Company</font>
			</td>
			<td colspan=1>
			<input type="text" name=Comp value="" size="23"></td>
                       <td>(Option)</td>

</tr>
	
                     	<tr>
			<td >
			<font class="sbd">EMail</font>
			</td>
			<td colspan=1>
			<input type="text" name="txtFrom" value="" size="23"></td>
                        <td><strong>(Required)</strong></td>                       

</tr>		
<tr>
	<td colspan=3 bgcolor="#eeeecc">
	<font class="f" size=2 color="#333333"><b>Account Information</b></font>
	</td>
</tr>
		

		<tr>
			<td >
			<font class="sbd"><a name="midpage">Password</a></font>
			</td>
			<td>
			<input type="password" name="txtPassword" value="" size="23" maxlength="12">
			</td>
			<td rowspan=2 bgcolor="#eeeedd">
			<font class="s">
			Must be <b>less than twelve (12) characters long</b>, may contain numbers (0-9) and upper and lowercase letters (A-Z, a-z), but <b>no spaces</b>. 
			</font>
			</td>
		</tr>
		<tr>
			<td >
			<font class="sbd">Re-enter Password</font>
			</td>
			<td>
			<input type="password" name="txtValidation" value="" size="23" maxlength="12">
			</tr>


                		<tr>
			<td colspan=5 bgcolor="#eeeedd">
			<font class="s">
			After you submit this registration form. Your userID and password will automatically be sent to the email address you provided. Make sure that you entered your email address correctly, otherwise, you will not be able to receive your userID and password,  and can't login our lakefish system.</b>
			</font>
			</td>
		</tr>
<tr>
            <td colspan=1>
            <INPUT TYPE=SUBMIT Value="Submit"></td>
            <td><p><a href="http://<%response.write(Application("name"))%>/lake/logint.asp"><font size=3>Go to Login if you have userID.</font></a>
	</td>

	</tr>
	
</table>
</html>
</body>
</html>