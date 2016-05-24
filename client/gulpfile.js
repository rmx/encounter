var gulp       = require('gulp')
  , ts         = require('gulp-typescript')
  , concat     = require('gulp-concat')
  , tslint     = require('gulp-tslint')
  , sourcemaps = require('gulp-sourcemaps')
  , react      = require('gulp-react')
  ;


var app = ts.createProject({
    declarationFiles: false,
    noExternalResolve: true,
    target: 'ES6',
    sortOutput: true,
    typescript: require('typescript')
});

gulp.task('lint', function() {
    return gulp.src(['app/**/*.ts', '!app/ext/**'])
        .pipe(tslint())
        .pipe(tslint.report('verbose'));
});

gulp.task('jsx', function() {
    return gulp.src('jsx/*.js')
        .pipe(react())
        .pipe(gulp.dest('tmp'));
});

gulp.task('default', function() {
    return gulp.src(['vendor/*.ts', 'config/boxen.ts', 'config/base.ts', 'config/base-help.ts', 'app/**/*.ts'], { base: '.' })
        .pipe(sourcemaps.init())
            .pipe(ts(app)).js
            .pipe(concat('entry.js'))
        .pipe(sourcemaps.write())
        .pipe(gulp.dest('tmp/'));
});

gulp.task('production', function() {
    return gulp.src(['vendor/*.ts', 'config/production.ts', 'config/base.ts', 
        'config/base-help.ts', 'config/fingerprints.ts', 'app/**/*.ts'], { base: '.' })
        .pipe(sourcemaps.init())
            .pipe(ts(app)).js
            .pipe(concat('entry.js'))
        .pipe(sourcemaps.write())
        .pipe(gulp.dest('tmp/'));
});

gulp.task('semilocal', function() {
    return gulp.src(['vendor/*.ts', 'config/semilocal.ts', 'config/base.ts', 
        'config/base-help.ts', 'app/**/*.ts'], { base: '.' })
        .pipe(sourcemaps.init())
            .pipe(ts(app)).js
            .pipe(concat('entry.js'))
        .pipe(sourcemaps.write())
        .pipe(gulp.dest('tmp/'));
});

gulp.task('watch', ['default', 'jsx'], function() {
    gulp.watch(['config/**/*.ts', 'app/**/*.ts'], ['default']);
    gulp.watch(['jsx/*.js'], ['jsx']);
});
