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
<!--<link rel="stylesheet" type="text/css" href="https://benjam.info/panam/styling.css">-->
</head>
<body>
<div id='monitor'><?=$value_html?></div>
    <script type='module'>
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

        if ('wakeLock' in navigator) {
            try {
                let wakeLock = await navigator.wakeLock.request('screen');
                console.log('wakeLock');
                console.log(wakeLock);
                document.addEventListener("visibilitychange", async () => {
                    if (document.visibilityState === 'visible') {
                        if (wakeLock === null || wakeLock.released === true) {
                            console.log('check lost');
                            try {
                                wakeLock = await navigator.wakeLock.request('screen');
                                console.log(wakeLock);
                            }
                            catch (err) {
                                console.log(err);
                                // alert(err);
                            }
                        }
                    }
                });
            }
            catch (err) {
                console.log(err);
                // alert(err);
            }
        }
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
$markdown = false;

if (isset($_REQUEST['value'])) {
    $value = $_REQUEST['value'];
    if (isset($_REQUEST['markdown'])) {
        $markdown = true;
        $descriptorspec = array(
            0 => array("pipe", "r"), 
            1 => array("pipe", "w"),
            2 => array("file", "/tmp/error-output.txt", "a")
        );
        $cwd = '/tmp';
        $env = array('some_option' => 'aeiou');
        $cmd = 'pandoc -f markdown-simple_tables-multiline_tables+pipe_tables -t html';
        /* $cmd = $cmd . ' --table-style=simple'; */
        $process = proc_open($cmd, $descriptorspec, $pipes, $cwd, $env);
        if (is_resource($process)) {
            fwrite($pipes[0], $value);
            fclose($pipes[0]);
            $html = stream_get_contents($pipes[1]);
            fclose($pipes[1]);
            proc_close($process);
            apcu_store($apcu_key, $html);
        }
    }
    else {
        apcu_store($apcu_key, $value);
    }
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
        <p>
        <span><input name='markdown' type='checkbox' id='cb1'/> </span>
            <span>Markdown</span>
        </p>
    </div>
    <div>
        <p>
            <input name='submit' type='submit' value="Submit (Ctrl+Enter)" id='submit' />
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
    var check = document.getElementById('cb1');
<?php
    if ($markdown) {
        echo "check.checked = true;\n";
    }
?>
}
</script>
</body>
</html>


