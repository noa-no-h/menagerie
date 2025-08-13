<?php
$filename = $_POST['filename'];
$handle = fopen($filename, "w"); //Open the file again, for WRITING this time
fwrite($handle, $_POST['arr']); // Write the new count to the file
fclose($handle); // Close the file
//echo $count; // Output to update the views live
?>