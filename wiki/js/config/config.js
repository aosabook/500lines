module.exports = {
  getStore: require('./config_db.js'), //specify config_db or config_file
  webserverport: 8080,
  viewsdir: './views' //relative to where npm start is launched
};
