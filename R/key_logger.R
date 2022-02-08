key_logger_script_SLS <- "
var time_points = [];
log_key_flag = true;
document.getElementById('marker_seq').style.visibility = 'hidden';
window.startTime = new Date().getTime();

window.onkeypress = register_key
console.log('Added keypress event listener, log_key_flag = ' + log_key_flag)

String.prototype.toMMSSZZ = function () {
    var msec_num = parseInt(this, 10); // don't forget the second param
    var sec_num = Math.floor(msec_num/1000);
    var milliseconds = msec_num - 1000 * sec_num;

    var hours   = Math.floor(sec_num / 3600);
    var minutes = Math.floor((sec_num - (hours * 3600)) / 60);
    var seconds = sec_num - (hours * 3600) - (minutes * 60);
    //if (hours   < 10) {hours   = '0' + hours;}
    //if (minutes < 10) {minutes = '0' + minutes;}
    //if (seconds < 10) {seconds = '0' + seconds;}
    return String(minutes).padStart(2, '0') + ':' + String(seconds).padStart(2, '0') + '.' + String(milliseconds).padStart(3, '0');
}
function register_key(e) {
  if(!log_key_flag){
    console.log('SLS Register key blocked')
    return;

  }
  var key = e.which || e.keyCode;
    console.log('SLS: Pressed key:' + key)
  if (key != 102 && key != 106) { // 'j' and 'f'
    // do nothing
    console.log('SLS: Invalid key')
    return;
  }
	var tp = new Date().getTime() - window.startTime
  time_points.push(tp);
  time_points.push(key);
  console.log('SLS: Time: ' + tp)
  log_key_flag = false;
  Shiny.setInputValue('marker_seq', time_points.join(':'));
  Shiny.onInputChange('next_page', performance.now())
}
"
key_proceed_script <- "
window.onkeypress = shiny_next

function shiny_next(e) {

  var key = e.which || e.keyCode;
  console.log('KPS: Pressed key:' + key)
  if (key != 102 && key != 106) { // 'j' and 'f'
    // do nothing
    console.log('KPS Invalid key')
    return
  }
  window.onkeypress = null;
  console.log('KPS: removed keypress event listener')
  Shiny.onInputChange('next_page', performance.now())
}
"

clean_up_script <- "
  window.onkeypress = null;

  //window.removeEventListener('keydown', register_key, false);
  console.log('CLUS: Removed keydown listener');
"
