<?php

function paramfault ()
{
	# xmlrpc-epi-php translates this into a real <fault>
	$fault["faultCode"] = -32602;
	$fault["faultString"] = "bad parameter";
	return $fault;
}

# We only check the params in sum(), because that's the one that
# xmlrpc-test tests will fail if given bad args

function sum ($method_name, $params, $app_data)
{
	if (xmlrpc_get_type ($params[0]) != "array")
		return paramfault();

	$sum = 0;
	foreach ($params[0] as $val) {
		if (xmlrpc_get_type ($val) != "int")
			return paramfault();
			
		$sum = $sum + $val;
	}
	return $sum;
}

function countBools ($method_name, $params, $app_data)
{
	$counts["true"] = $counts["false"] = 0;
	foreach ($params[0] as $val) {
		if ($val)
			$counts["true"] = $counts["true"] + 1;
		else
			$counts["false"] = $counts["false"] + 1;
	}
	return $counts;
}

function md5sum ($method_name, $params, $app_data)
{
	$val = md5 ($params[0]->scalar, true);
	xmlrpc_set_type ($val, "base64");
	return $val;
}

function dateChange ($method_name, $params, $app_data)
{
	$date_str = $params[0]->scalar;
	$date = strptime ($date_str, "%Y%m%dT%H:%M:%S");

	foreach ($params[1] as $name => $val) {
		if ($name == "date")
			continue;
		$date[$name] = $val;
	}

	$ret = sprintf ("%04d%02d%02dT%02d:%02d:%02d",
			$date["tm_year"] + 1900, $date["tm_mon"] + 1,
			$date["tm_mday"], $date["tm_hour"],
			$date["tm_min"], $date["tm_sec"]);
	xmlrpc_set_type ($ret, "datetime");
	return $ret;
}

function echo_ ($method_name, $params, $app_data)
{
	return $params[0];
}

# Work around xmlrpc-epi-php lossage; otherwise the datetime values
# we return will sometimes get a DST adjustment we don't want.
putenv ("TZ=");

$xmlrpc_server = xmlrpc_server_create ();
xmlrpc_server_register_method($xmlrpc_server, "sum", "sum");
xmlrpc_server_register_method($xmlrpc_server, "countBools", "countBools");
xmlrpc_server_register_method($xmlrpc_server, "md5sum", "md5sum");
xmlrpc_server_register_method($xmlrpc_server, "dateChange", "dateChange");
xmlrpc_server_register_method($xmlrpc_server, "echo", "echo_");

$response = xmlrpc_server_call_method ($xmlrpc_server,
				       implode("\r\n", file('php://input')),
				       0, array ("output_type" => "xml"));
echo ($response);

xmlrpc_server_destroy ($xmlrpc_server);

?>
