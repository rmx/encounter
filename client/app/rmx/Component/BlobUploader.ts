/// <reference path="./Base.ts" />
/// <reference path="../data.ts" />

declare var classNames;

module rmx.Component {

    // file is of W3C type File
    function mimeType(filename) {
        var mimeTypes =
            { '.png$'  : 'image/png'
            , '.jpg$'  : 'image/jpeg'
            , '.jpeg$' : 'image/jpeg'
            , '.svg$'  : 'image/svg+xml'
            , '.json$' : 'application/json'
            };

        for (var re in mimeTypes) {
            if (filename.match(new RegExp(re, 'i'))) {
                return mimeTypes[re];
            }
        }

        console.warn('Could not detect content type of file', filename);
        return 'application/octet-stream';
    }


    export interface BlobUploaderProps {
        object : any;
        field  : string;
    }

    interface BlobUploaderState {
        filename    ?: string;
        xhr         ?: any;
        classList   ?: string;
        buttonLabel ?: string;
    }

    class BlobUploaderSpec extends ReactComponent<BlobUploaderProps, BlobUploaderState> {

        getInitialState() {
            return { filename    : ''
                   , xhr         : null
                   , classList   : 'inactive'
                   , buttonLabel : 'Select a file'
                   };
        }

        render() {
            var classList = { rmx: 1, blob: 1, upload: 1 };
            classList[this.state.classList] = 1;
            var className = classNames(classList);

            return React.DOM.div
                ( { className: className }
                , React.DOM.input
                    ( { ref: 'file', type: 'file', style: { display: 'none' }, onChange: this.fileSelected }
                    )
                , React.DOM.div
                    ( { className: 'progress' }
                    , React.DOM.div
                        ( { ref: 'bar', className: 'bar' }
                        , React.DOM.span({ id: 'fileName' }, this.state.filename)
                        )
                    )
                , React.DOM.button
                    ( { className: 'button', onClick: this.buttonClick }
                    , this.state.buttonLabel
                    )
                );
        }

        buttonClick() {
            if (!(<any>this.refs['file'].getDOMNode()).value) {
                (<any>this.refs['file'].getDOMNode()).click();
            } else if (this.state.xhr) {
                this.state.xhr.abort();
                this.setState({ xhr: null, buttonLabel: 'Start upload' });
            } else {
                this.startUpload();
            }
        }

        fileSelected(e) {
            this.setState({
                filename: e.target.value.replace(/c:\\fakepath\\/i, ''),
                buttonLabel: 'Start upload',
                classList: 'reatd'
            });
        }

        startUpload() {
            var self = this;

            this.setState({
                classList: 'uploading',
                buttonLabel: 'Uploading ...'
            });

            var bar = <any> this.refs['bar'].getDOMNode();
            bar.style.display = 'block';
            bar.style.width = 0;

            var file    = (<any>this.refs['file'].getDOMNode()).files[0];
            var promise = <any> rmx.data.createBlob(file, mimeType(this.state.filename));
            this.setState({ xhr: promise.xhr });

            promise.xhr.upload.addEventListener("progress", onUploadProgress, false);
            promise.xhr.upload.addEventListener("load",     onUploadDone,     false);

            promise.then(blobId => {
                this.props.object[this.props.field] = blobId;

                this.setState({
                    filename: '',
                    buttonLabel: 'Select file',
                    xhr: null,
                    classList: 'inactive'
                });

                (<any>this.refs['file'].getDOMNode()).value = null;

                bar.style.display = 'none';

            }).catch(err => {
                console.log(err);

                this.setState({
                    filename: '',
                    buttonLabel: 'Select file',
                    xhr: null,
                    classList: 'inactive'
                });

                (<any>this.refs['file'].getDOMNode()).value = null;

                bar.style.display = 'none';
            });

            function onUploadProgress(ev) {
                if (ev.lengthComputable) {
                    var percentage = Math.round((ev.loaded * 100) / ev.total);
                    bar.style.width = percentage + '%';
                }
            }

            function onUploadDone(ev) {
                bar.style.width = '100%';
                self.setState({ buttonLabel: 'Processing ...' });
            }
        }

    }

    export var BlobUploader = createClass(BlobUploaderSpec);
}
