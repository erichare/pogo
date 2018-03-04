var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');
var path = 'full_counters.html'

page.open('https://pokemongo.gamepress.gg/raid-boss-counters', function (status) {
  var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
});
