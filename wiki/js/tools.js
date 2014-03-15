exports.parseJSON = function(data, callback){
  try{
    return callback(null, JSON.parse(data));
  }catch(e){
    return callback(e, null);
  }
};
