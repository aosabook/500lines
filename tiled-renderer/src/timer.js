var timer = new function() {
    this.totalFrames = 0;
    this.totalMS = 0;

    this.tick = function(timestamp) {
        if (this.last == null) {
            this.reset(timestamp);
            return;
        }
        var elapsedMS = timestamp - this.last;
        this.totalMS += elapsedMS;
        ++this.frames;
        ++this.totalFrames;
        this.last = timestamp;
        if (this.frames % 32 == 0) this.reset(timestamp);
    };

    this.fps = function() {
        return this.rollingAvgFPS;
    };

    this.reset = function(timestamp) {
        this.rollingAvgFPS = this.totalMS == 0 ? 0 : this.frames / (this.totalMS / 1000);
        this.throttleValue = this.rollingAvgFPS < 40 ? 30 : 60;
        this.rollingAvgFPS = this.rollingAvgFPS < this.throttleValue * 0.95 ? this.rollingAvgFPS : this.throttleValue;
        this.last = timestamp;
        this.frames = 0;
        this.totalMS = 0;
    };
};
