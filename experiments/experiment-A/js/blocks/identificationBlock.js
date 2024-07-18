/*
 * Author: Dave F. Kleinschmidt
 *
 *    Copyright 2012 Dave Kleinschmidt and
 *        the University of Rochester BCS Department
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU Lesser General Public License version 2.1 as
 *    published by the Free Software Foundation.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public License
 *    along with this program.
 *    If not, see <http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html>.
 *
 */
var lastplayed = "";
if (typeof String.prototype.startsWith != 'function') {
  // see below for better implementation!
  String.prototype.startsWith = function (str){
    return this.indexOf(str) == 0;
  };
}

function IdentificationBlock(params) {
    // process parameters
    var stimuliObj, instructions, namespace, css_stim_class;
    for (p in params) {
        switch(p) {
        case 'stimuli':
            stimuliObj = params[p];
            break;
        case 'instructions':
            instructions = params[p];
            break;
        case 'namespace':
            namespace = params[p];
            break;
        case 'reps':
            this.reps = params[p];
            break;
        case 'blockReps':
            this.blockReps = params[p];
            break;
        case 'blockRandomizationMethod':
            this.blockRandomizationMethod = params[p];
            break;
        case 'ITI':
            this.ITI = params[p];
            break;
        case 'respKeys':
            this.respKeys = params[p];
            break;
        case 'categories':
            this.categories = params[p];
            break;
        default:
            break;
        }
    }

    // set namespace for this block (prefix for events/form fields/etc.) and
    // the class that the stimuli are assigned
    if (typeof(namespace) === 'undefined') {
        var namespace = '';
        var css_stim_class = 'stim';
    } else {
        var css_stim_class = namespace + 'stim';
        this.namespace = namespace;
    }

    // install stimuli
    if (typeof(stimuliObj) === 'undefined') {
        console.log("ERROR: unrecognized stimulus object.")
    }

    if (isArray(stimuliObj)) {
        // concatenate into one mega-object, and set for this block
        // NOTE / TODO: I'm uncertain if this part of the loop works... when would you need it?
        this.stimuliObj = concatenate_stimuli_and_install(stimuliObj, css_stim_class);
        this.auds = this.stimuliObj.installed;
    } else {
        // set stimuli object for this block
        this.stimuliObj = stimuliObj;
        stimuliObj.get_and_load_stims(css_stim_class); // this is asynchronous
        $('#continue').show();
    }

    // create responses form element and append to form
    this.respField = $('<textArea id="' + namespace + 'Resp" ' +
                       'name="' + namespace + 'Resp" ></textArea>').appendTo('#mturk_form');
    $('#mturk_form').append('<br />');

}


IdentificationBlock.prototype = {
    reps: undefined,
    blockReps: 1,
    stims: [],
    n: 0,
    respKeys: undefined, // {71: 'B', 72: 'D'},
    categories: undefined, // ['B', 'D']
    ncorrect: 0,
    keyCapture: false,
    tResp: -1,
    tStart: -1,
    ITI: 1000,
    auds: [],
    practiceMode: false,
    namespace: '',
    isCatchTrial: undefined,
    catchAns: false,
    respField: undefined,
    onEndedBlock: undefined,
    pbIncrement: undefined,
    blockRandomizationMethod: undefined,

    getTotalReps: function() {
        var reps;
        var blockReps = 1;
        if (typeof this.reps === 'undefined')  {
            reps = this.stimuliObj.calibReps;
        } else {
            reps = this.reps;
        }

        if (typeof this.blockReps !== 'undefined') {
            blockReps = this.blockReps;
        }

        if (reps.sum) {
            return(reps.sum());
        } else {
            return(reps * blockReps * this.stimuliObj.continuum.length);
        }
    },

    run: function() {
        var _self = this;
        this.init();
       _self.next();
    },

    init: function(opts) {
        var _self = this;
        // takes the stimuli that was loaded asynchronology and matches it with the proper block. Note that each block you installed should
        // have a different namespace so this filters properly.
        var temp = _self.stimuliObj.get_installed();
        var temp2 = []
        for (var i=0; i<temp.length; i++) {
            if (temp[i].class.startsWith(_self.namespace + "stim")) {
                temp2.push(temp[i]);
            }
        }

        // temp = temp.filter(function(i) {
        //    return i.getAttribute('class').startsWith( _self.namespace + "stim");
        //  });
        _self.auds = temp2;
        _self.stims = temp2;

        var softInit;

        // parse optional arguments object
        this.practiceMode = false;

        for (op in opts) {
            switch (op) {
            case 'practiceMode':
                this.practiceMode = opts[op];
                break;
            case 'softInit':
                softInit = true;
                break;
            default:
                if (console) console.log('Unrecognized Block init option: ' +
                                         op + ' (' + opts[op] + ')');
            }
        }

        // initialize trial counter
        this.n = 0;

        ////////////////////////////////////////////////////////////////////////////////
        // initialize response keys and response labels:
        // response keys can be provided to constructor, or default to global var respKeyMap
        if (typeof this.respKeys === 'undefined') {
            this.respKeys = respKeyMap;
        }

        // likewise response labels ('categories') can be provided to the constructor or
        // set from the global (if it exists), or default to being extracted from the values
        // of the response key mapping.
        if (typeof this.categories === 'undefined') {
            // populate the category names from the global vector if it exists, or extract from the resp keys
            if (typeof categories === 'undefined') {
                this.categories = [];
                for (k in this.respKeys) {
                    this.categories.push(this.respKeys[k]);
                }
            } else {
                this.categories = categories;
            }
            // console.log(this.keyCapture);
            // console.log(this.respKeys[String.fromCharCode(e.which)]);
        }

        if (!validateRespKeys(this.respKeys, this.categories)) {
            return false;
        }


        ////////////////////////////////////////////////////////////////////////////////
        // Randomize stimuli order.
        // default to "calibReps" reps property of this.stimuliObj for reps of each
        // stimulus.
        if (typeof(this.reps) === 'undefined') {
            this.reps = this.stimuliObj.calibReps;
        }

        this.stims = [];
        for (var br = 0; br < this.blockReps; br++) {
            this.stims = this.stims.concat(orderStimuli(this.reps, this.stimuliObj.continuum.length, this.blockRandomizationMethod));
        }

        this.pbIncrement = 1.0 / this.stims.length;

        ////////////////////////////////////////////////////////////////////////////////
        // Bind handlers for this block:
        // create handler to capture and process keyboard input, and bind to document
        $(document).bind('keyup.' + this.namespace, function(e) {_self.handleResp(e);});

        // create handler to turn on keyboard capture when stims end, and bind to stims
        // $(this.auds).bind('ended.' + this.namespace, function() {_self.waitForResp();});

        ////////////////////////////////////////////////////////////////////////////////
        // Initialize UI elements
        // set task instructions and response cues
        $("#taskInstructions").html('Press <span id="bKey" class="respKey">' +
                                    valToKey(this.respKeys, this.categories[0]) +
                                    '</span> for "' + this.categories[0] + '"<br />' +
                                    'Press <span id="dKey" class="respKey">' +
                                    valToKey(this.respKeys, this.categories[1]) + '</span> for "' + this.categories[1] + '"');

        if (!softInit) {
            // install, initialize, and show a progress bar (progressBar.js)
            installPB("progressBar");
            resetPB("progressBar");
            $("#progressBar").show();
            // DEBUGGING: add button to force start of calibration block (skip preview)
            $('#buttons').append('<input type="button" onclick="calibrationBlock.next()" value="start calibration"></input>');
        }
    },

    waitForResp: function() {
        this.keyCapture=true;
        $("#fixation").hide();
        $("#taskInstructions").removeClass("dimmed");
        $('#testStatus').html('Stim ended');
    },

    handleResp: function(e) {
        $('#testStatus').html('keyup() detected: keycode = ' + e.which + ' (' +
                              String.fromCharCode(e.which) + ')');

        if (String.fromCharCode(e.which) == 'B') {
            throwMessage("Catch response entered.");
            $("#whiteDotFeedback").hide();
            $("#whiteDotFeedbackTrue").show();
            this.catchAns = true;
            e.preventDefault();

            // uncomment if pressing catch key should NOT end the trial
            // return false;
            this.tResp = Date.now();
            this.keyCapture=false;
            this.end(e);
        }

        if (this.keyCapture && this.respKeys[String.fromCharCode(e.which)]) {
            this.tResp = Date.now();
            this.keyCapture=false;
            this.end(e);
        }
    },

    // start next trial
    next: function() {
        var _self = this;
        // some status information (hidden by default)
        $('#testStatus').append('<br />stims: ' + this.stims + ', n: ' + this.n);
        $('#testStatus').html('...wait');

        $("#taskInstructions").show().addClass("dimmed");
        $("#whiteDotFeedback").show().addClass("dimmed");
        $("#whiteDotFeedbackTrue").hide();
        // pause before next fixation cross appears
        setTimeout(function() {
                     $("#fixation").show();
                     }, _self.ITI/2);
        // play next stimuli after ITI has elapsed (asynchronously with fixation display)
        setTimeout(function() {
                         // NOTE: can't use 'this' here, since function is being evaluate outside of method context
                         var current = _self.auds[_self.stims[_self.n]];
                         console.log(current);
                         if (current.type == 'video') {
                            var video = document.createElement('video');
                            video.src = current.src;
                            video.setAttribute('class', current.class);
                            document.body.children.videoContainer.appendChild(video);

                            lastplayed = current.src.split("/").pop();
                            _self.isCatchTrial = lastplayed.split(".")[0].indexOf("CATCH") > -1;
                            video.play();
                            $(video).bind('ended.' + this.namespace, function() {_self.waitForResp();});
                            video = null;
                         }
                         if (current.type == 'audio') {
                            var audio = document.createElement('audio');
                            audio.src = current.src;
                            audio.setAttribute('class', current.class);
                            document.body.children.audioContainer.appendChild(audio);
        // create handler to turn on keyboard capture when stims end, and bind to stims
                            audio.play();
                            lastplayed = current.src.split("/").pop();
                            $(audio).bind('ended.' + this.namespace, function() {_self.waitForResp();});
                            audio = null;
                         }
                         _self.tStart = Date.now();
                         $('#testStatus').html('Trial started');
                     }, _self.ITI);
    },

    // handle end of trial (called by key press handler)
    end: function(e) {
        //This gets rid of the problem of div creeping...
        $('video').eq(0).remove();
        $('audio').eq(0).remove();
        $('#testStatus').html('Trial ended');
        // update progress bar
        plusPB("progressBar", this.pbIncrement);
        // record response
        this.recordResp(e);
        this.isCatchTrial = undefined;
        this.catchAns = false;
        // if more trials remain, trigger next trial
        if (++this.n < this.stims.length) {
            this.next();
        } else {
            this.endBlock();
        }
    },

    endBlock: function() {
        // trigger endCalibrationBlock event
        $("#taskInstructions").hide();
        $("#whiteDotFeedback").hide();
        $("#whiteDotFeedbackTrue").hide();
        $("#progressBar").hide();
        $(this.auds).unbind('.' + this.namespace).height(0);
        $(document).unbind('.' + this.namespace);
        $(document).trigger('endBlock_' + this.namespace +
                            (this.practiceMode ? '.practice' : ''));
        if (this.practiceMode && typeof(this.onEndedPractice) === 'function') {
            this.onEndedPractice();
        } else if (typeof(this.onEndedBlock) === 'function') {
            this.onEndedBlock();
        } else {
            if (console) console.log('WARNING: End of block reached but no callback found');
        }
    },

    // return info on current state in string form
    info: function() {
        // alert('stims: ' + this.stims + ', n: ' + this.n);
        return [this.namespace + (this.practiceMode ? '.practice' : ''),
                this.n, this.stims[this.n], lastplayed].join();
    },

    // method to handle response. takes event object as input
    recordResp: function(e) {

        // format trial information
        var resp = [this.info(), e.which,
                    this.respKeys[String.fromCharCode(e.which)],
                    this.tStart, this.tResp, this.tResp-this.tStart, this.isCatchTrial, this.catchAns].join();
        // write info to form field
        //$('#calibrationResp').val($('#calibrationResp').val() + resp + RESP_DELIM);
        $(this.respField).val($(this.respField).val() + resp + RESP_DELIM);
    },

    demo: function() {
        // function to demonstrate categories.

    },

    practice: function(parameters, callback) {
        // start a short practice "block"
        this.init({practiceMode: true});
        this.onEndedPractice = callback;

        $("#progressBar").hide();
        var contMax = this.stimuliObj.continuum.length - 1;
        this.stims = [0,contMax,0,contMax];

        $("#instructions").html('<h3>Task: practice</h3>' +
                                (typeof parameters['instructions'] === 'undefined' ? '' : parameters['instructions']) +
                                '<p>In this task, you will listen to words, and be asked to decide if they contain a "' + this.categories[0] + '" or a "' + this.categories[1] + '".  Press <span class="respKey">' + valToKey(this.respKeys, this.categories[0]) + '</span> for ' + this.categories[0] + ' and <span class="respKey">' + valToKey(this.respKeys, this.categories[1]) + '</span> for ' + this.categories[1] + '.</p>').show();

        var _self = this;
        continueButton(function() {
                           $('#instructions').hide();
                           _self.next();
                       });
    }
};



// classical-esque class inheritance: sets prototype of prototype to superclass prototype
function extend(child, supertype)
{
    child.prototype.__proto__ = supertype.prototype;
    child.prototype.__super__ = supertype.prototype;
}



// reverse map lookup (get key given value)
function valToKey(obj, v) {
    var keys = [];
    for (k in obj) {
        if (obj[k]==v) {
            keys.push(k);
        }
    }
    return(keys);
}


function validateRespKeys(respKeys, categories) {
    for (k in respKeys) {
        if (! categories.has(respKeys[k])) {
            if (console) console.log('ERROR: response label {0} not found in specified categories {1}'.format(respKeys[k], categories));
            return false;
        }
    }
    return true;
}
