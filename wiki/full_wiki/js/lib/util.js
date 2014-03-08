const marked = require('marked'),
      dateformat = require('dateformat');

module.exports = {

  processHtml: function(wikiMarkup){
    return marked(wikiMarkup);
  },

  getSeparator: function(path){
    //if path already ends in /, return empty string, otherwise return /
    return /\/$/.test(path) ? '': '/';
  },

  formatDate: function(date){
    return dateformat(date, "h:MMTT d-mmm-yyyy");
  }


}
