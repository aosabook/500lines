var timer = (function() {
    'use strict';
    var instance = {};

    instance.totalFrames = 0;
    instance.totalMS = 0;

    instance.tick = function(timestamp) {
        if (!this.last) {
            this.reset(timestamp);
            return;
        }
        var elapsedMS = timestamp - this.last;
        this.totalMS += elapsedMS;
        ++this.frames;
        ++this.totalFrames;
        this.last = timestamp;
        if (this.frames % 32 === 0) this.reset(timestamp);
    };

    instance.fps = function() {
        return this.rollingAvgFPS;
    };

    instance.reset = function(timestamp) {
        this.rollingAvgFPS = this.totalMS === 0 ? 0 : this.frames / (this.totalMS / 1000);

        // Use a 5% buffer around refresh rate throttle values (30fps and
        // 60fps) to account for clock skew and other variables which could
        // destabilize the number.
        this.throttleValue = this.rollingAvgFPS < 40 ? 30 : 60;
        this.rollingAvgFPS = this.rollingAvgFPS < this.throttleValue * 0.95 ? this.rollingAvgFPS : this.throttleValue;
        this.last = timestamp;
        this.frames = 0;
        this.totalMS = 0;
    };

    return instance;
})();
