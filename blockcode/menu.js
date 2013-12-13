(function(global){
	var menu = document.querySelector('.menu');
	function menuItem(name, fn){
		var item = document.createElement('div');
		item.className = 'block';
		item.setAttribute('draggable', 'true');
		item.textContent = name;
		item.addEventListener('run', function(evt){
			this.classList.add('running');
			fn(this);
			this.classList.remove('running');
		}, false);
		menu.appendChild(item);
	}

})(window);