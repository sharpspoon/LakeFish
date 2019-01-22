<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
  <title>Untitled Document</title>
                
  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
</head>
 <body bgcolor="#ffffff">
<p align="Center"><b>Please enter the new fish species:</b></p>
<form method="Post" action="http://<%response.write(Application("name"))%>:8080/servlet/SpeciesGroup">
  <table width="90%" border="0" cellspacing="0" cellpadding="0">
    <tbody>
      <tr>
        <td width="61%">
        <div align="Right">Species:</div>
        </td>
        <td width="39%">
        <input type="text" name="SpeciesGroup" value="New Species" size="20"></td>
      </tr>
      <tr>
        <td width="61%">
        <div align="Right">Lower Good-growth Temperature Limit(LGGT):</div>
        </td>
        <td width="39%"><input type="text" name="lggt" value="0.0" size="20"></td>
      </tr>
      <tr>
        <td width="61%">
        <div align="Right">Upper Good-growth Temperature Limit(UGGT):</div>
        </td>
        <td width="39%"><input type="text" name="uggt" value="0.0" size="20"></td>
      </tr>
      <tr>
        <td width="61%">
        <div align="Right">Lethal Temperature Threshold(LT):</div>
        </td>
        <td width="39%"><input type="text" name="lt" value="0.0" size="20"></td>
      </tr>
      <tr>
        <td width="61%">
        <div align="Right">Species group:</div>
        </td>
        <td width="39%">
        <select name="fishclass">
        <option value="cool" selected="">cool</option>
        <option value="cold">cold</option>
        <option value="warm">warm</option>
        </select>
        </td>
      </tr>
    </tbody>
  </table>
  <p align="Center"><input type="submit" name="Submit" value="Submit"><input type="reset" name="Clear" value="Clear"></p>
</form>
<p>&nbsp; </p>
</body>
</html>