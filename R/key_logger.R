key_logger_script_SLS <- "
var time_points = [];

document.getElementById('marker_seq').style.visibility = 'hidden';
window.startTime = new Date().getTime();

window.onkeypress = register_key
console.log('Added keypress event listener')

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

  var key = e.which || e.keyCode;
    console.log('Pressed key:' + key)
  if (key != 102 && key != 106) { // 'j' and 'f'
    // do nothing
    console.log('Invalid key')
    return;
  }
	var tp = new Date().getTime() - window.startTime
  time_points.push(tp);
  time_points.push(key);
  console.log('Time: ' + tp)
  Shiny.setInputValue('marker_seq', time_points.join(':'));
  Shiny.onInputChange('next_page', performance.now())
}
"
key_proceed_script <- "
window.onkeypress = shiny_next

function shiny_next(e) {

  var key = e.which || e.keyCode;
  console.log('Pressed key:' + key);
  //if (key != 102 && key != 106) { // 'j' and 'f'
    // do nothing
    //console.log('Invalid key')
  //  return
  //}
  Shiny.onInputChange('next_page', performance.now())
}
"

clean_up_script <- "
  window.onkeypress = null;

  //window.removeEventListener('keydown', register_key, false);
  console.log('Removed keydown listener');
"
