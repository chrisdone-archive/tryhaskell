<?php
ob_start("ob_gzhandler");
if (preg_match('/.css/',$_GET['file'])) {
  header("Content-Type: text/css");
  $_GET['file'] = preg_replace('/[.]+/','.',$_GET['file']);
  readfile('.'.$_GET['file']);
}
else if (preg_match('/.js/',$_GET['file'])) {
  $_GET['file'] = preg_replace('/[.]+/','.',$_GET['file']);
  readfile('.'.$_GET['file']);
}
?>