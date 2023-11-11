<?php
#======================================================================
#
# monitor.php - monitor text on mobile devices
#
# Created by skywind on 2023/11/10
# Last Modified: 2023/11/10 23:44
#
#======================================================================

#----------------------------------------------------------------------
# init
#----------------------------------------------------------------------
header("Access-Control-Allow-Origin: *");

$action = isset($_REQUEST['action'])? $_REQUEST['action'] : "";
$key = isset($_REQUEST['key'])? $_REQUEST['key'] : 'default';
$apcu_key = "MONITOR_" . $key;
$script = $_SERVER['SCRIPT_NAME'];


#----------------------------------------------------------------------
# set
#----------------------------------------------------------------------
if ($action == 'set') {
    if (isset($_REQUEST['value'])) {
        $value = $_REQUEST['value'];
        apcu_store($apcu_key, $value);
        echo "OK";
    }
    exit;
}


#----------------------------------------------------------------------
# query
#----------------------------------------------------------------------
if ($action == 'get') {
    $value = apcu_fetch($apcu_key);

    if ($value === false) {
        echo "empty content";
        exit;
    }

    echo $value;
    exit;
}


#----------------------------------------------------------------------
# test 
#----------------------------------------------------------------------
if ($action == 'test') {
    header('content-type: text/plain;charset=utf-8');
    echo "url: $script\n";
    echo "suck\n";
    $protocol = isset($_SERVER['HTTPS']) && $_SERVER['HTTPS'] === 'on' ? "https" : "http";
    $current_url = "$protocol://$_SERVER[HTTP_HOST]$_SERVER[REQUEST_URI]";
    echo $current_url;
    exit;
}


#----------------------------------------------------------------------
# QRCode
#----------------------------------------------------------------------
if ($action == 'qrcode') {
    $protocol = isset($_SERVER['HTTPS']) && $_SERVER['HTTPS'] === 'on' ? "https" : "http";
    $current_url = "$protocol://$_SERVER[HTTP_HOST]$_SERVER[SCRIPT_NAME]";
    $url = $current_url . '?action=display&key=' . rawurlencode($key);
?>
<html>
<head>
<title>Monitor - QRCode</title>
</head>
<body>
    <div>
        <p>
            url: <?=$url?>
        </p>
    </div>
    <div id="qrcode"></div>
    <script src='https://unpkg.com/qrcodejs@1.0.0/qrcode.min.js'></script>
    <script>
        new QRCode(document.getElementById("qrcode"), "<?=$url?>");
    </script>
</body>
</html>
<?php
    exit;
}


#----------------------------------------------------------------------
# display
#----------------------------------------------------------------------
if ($action == 'display') {
    $value = apcu_fetch($apcu_key);
    if ($value === false) {
        $value = '';
    }
    $value_html = htmlspecialchars($value);
    $url = $script . '?action=get&key=' . rawurlencode($key);
?>
<html>
<meta name="viewport" content="width=device-width" />
<head>
<title>Monitor - Display</title>
</head>
<body>
<div id='monitor'><?=$value_html?></div>
    <script>
        function RefreshContent() {
            var monitor = document.getElementById('monitor');
            var url = '<?=$url?>';
            fetch(url)
                .then(response => response.text())
                .then(data => {
                    monitor.innerHTML = data;
                })
                .catch(error => {
                    console.error('request failed', error);
                })
                .finally(() => {
                    setTimeout(RefreshContent, 500);
                });
        }
        RefreshContent();
    </script>
</body>
</html>
<?php
    exit;
}


#----------------------------------------------------------------------
# edit
#----------------------------------------------------------------------
$value = '';
if (isset($_REQUEST['value'])) {
    $value = $_REQUEST['value'];
    apcu_store($apcu_key, $value);
}
else {
    $value = apcu_fetch($apcu_key);
    if ($value === false) {
        $value = '';
    }
}
$value_html = htmlspecialchars($value);
$url = $script . '?action=display&key=' . rawurlencode($key);
$url2 = $script . '?action=qrcode&key=' . rawurlencode($key);
?>
<html>
<head><title>Monitor - Edit</title></head>
<body>
<div>
    <form method='post' action="<?=$script?>">
    <input type="hidden" id="hidden1" name='action' value='edit'>
    <div>
        <p>Key:</p>
        <p>
            <input name='key' type='text' value='<?=$key?>' />
        </p>
    </div>
    <div>
        <p>Content:</p>
        <p>
            <textarea name='value' id='myta' cols=80 rows=24><?=$value_html?></textarea>
        </p>
    </div>
    <div>
        <p>
            <input name='submit' type='submit' value="submit" id='submit' />
        </p>
    </div>
    </form>
</div>
<div>
    <p>
        <a href='<?=$url?>' target='_blank'>display</a>
    </p>
    <p>
        <a href='<?=$url2?>' target='_blank'>QRCode</a>
    </p>
</div>
<script>
window.onload = function() {
    var textarea = document.getElementById('myta');
    var button = document.getElementById('submit');
    textarea.focus();
    textarea.selectionStart = textarea.value.length;
    textarea.selectionEnd = textarea.value.length;
    textarea.addEventListener('keydown', function(event) {
        if (event.ctrlKey && event.key == 'Enter') {
            button.click();
        }
    });
}
</script>
</body>
</html>


