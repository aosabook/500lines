var timer = new function() {
    this.totalFrames = 0;

    this.tick = function(timestamp) {
        if (this.last == null) {
            this.reset(timestamp);
            return;
        }
        var elapsedMS = timestamp - this.last;
        this.totalMS += elapsedMS;
        this.frames += 1;
        this.totalFrames += 1;
        this.last = timestamp;
        if (this.frames % 1000 == 0) {
            console.log('FPS(' + this.frames + '): ' + this.fps());
            this.reset(timestamp);
        }
    };

    this.fps = function() {
        return this.frames / (this.totalMS / 1000);
    };

    this.reset = function(timestamp) {
        this.last = timestamp;
        this.frames = 0;
        this.totalMS = 0;
    };
};
