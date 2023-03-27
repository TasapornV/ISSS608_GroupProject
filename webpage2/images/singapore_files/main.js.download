(function( $ ){
    "use strict";
    var $win = jQuery( window ),
        $doc = jQuery(document),
        $htmlBody = jQuery('html,body'),
        $body = jQuery( document.body ),
    registerDependencies = function() { 
        var dependencies = null != window.exponentThemeConfig ? window.exponentThemeConfig.dependencies : {};
        if( null != dependencies ) {
            for( var dependency in dependencies ) {
                if( dependencies.hasOwnProperty( dependency ) ) {
                    asyncloader.register( dependencies[ dependency ], dependency );
                }
            }
        }
        asyncloader.register( "https://player.vimeo.com/api/player.js", 'vimeonew' );
    },
    grid = function() {
        asyncloader.require( [ 'isotope', 'begrid' ], function() {
            var grids = jQuery( '.be-grid[data-layout="metro"],.be-grid[data-layout="masonry"]' );
            grids.each( function() {
                new BeGrid(this);
            });
        });
    },
    comments = (function() {
        var commentsLine = function() {
                if( $body.hasClass( 'single-post' ) ) {
                    var commentsWithReplies = jQuery( '.exp-comment-parent' );
                    if( 0 < commentsWithReplies.length ) {
                        if( 959 < $win.width() ) {
                            commentsWithReplies.each( function() {
                                var curComment = jQuery(this),
                                    repliesOrRespond,
                                    authorImage,
                                    repliesOrRespondHeight = 0,
                                    top,
                                    left,
                                    height,
                                    width,
                                    commentFollowLine = curComment.find( '.exp-comment-follow-line' );
                                if( 0 < commentFollowLine.length ) {
                                    repliesOrRespond = curComment.nextUntil( '.comment' );
                                    if( 0 < repliesOrRespond.length ) {
                                            repliesOrRespond.each(function( i, el ) {
                                                repliesOrRespondHeight += $(this).outerHeight( i == (repliesOrRespond.length - 1) ? false : true );
                                            });
                                            authorImage = curComment.find( '.exp-comment-author-image' );
                                        if( 0 < authorImage.length ) {
                                            top = authorImage.height() + 32;
                                            left = authorImage.width()/2;
                                            height = ( curComment.outerHeight( true ) + repliesOrRespondHeight ) - top;
                                            width = 1;
                                            commentFollowLine.css({
                                                top : top + 'px',
                                                left : left + 'px',
                                                height : height + 'px',
                                                width : width + 'px',
                                                display : 'block'
                                            });
                                        }
                                    }
                                }
                            });
                        }else {
                            commentsWithReplies.find( '.exp-comment-follow-line' ).css( 'display', 'none' );
                        }
                    }
                }
            };
        $( document ).on( 'click', '.exp-comment-details', function() {
            var curReply = $(this),
                replyParent = curReply.closest( '.comment' ),
                respond = $( '#respond' ),
                replyMargin = 0,
                replyDetails = replyParent.find( '.exp-comment-details' );
            if( 0 < replyParent.length && 0 < replyDetails.length && 0 < respond.length ) {
                replyMargin = replyDetails.offset().left - replyParent.offset().left;
                respond.css( 'margin-left', replyMargin + 'px' );
                commentsLine();
            }
        } );
        $( document ).on( 'click', '#cancel-comment-reply-link', function() {
            var respond = $( '#respond' );
            if( 0 < respond.length ) {
                respond.css( 'margin-left', '' );
                commentsLine();
            }
        } );
        return commentsLine;
    })(),
    expose = function() {
        var BeScripts = {
            comments : comments
        };
        window.BeScripts = BeScripts;
    },
    video = function() {
        var vimeoVideos = $( '.be-vimeo-embed' ),
            youtubeVideos = $( '.be-youtube-embed' ),
            iframeEle = $( 'iframe' ),
            loadYoutubeVideos,
            videoReadyCallback;
        videoReadyCallback = function( iframeEle ) {
            asyncloader.require( ['fitvids'], function(){
                if( null != iframeEle && 0 < iframeEle.length ) {
                    iframeEle.closest( '.be-video-embed' ).removeClass( 'be-embed-placeholder' );
                    iframeEle.parent().fitVids();
                    $(document).trigger( 'be_video_loaded', [ iframeEle ] );
                }  
            });        
        };
        loadYoutubeVideos = function() {
            youtubeVideos.each(function() {
                var curVideo = $(this),
                    curPlayer = null,
                    id = null != curVideo.attr( 'data-video-id' ) ? curVideo.attr( 'data-video-id' ) : null,
                    autoplay = null != curVideo.attr( 'data-autoplay' ) ? parseInt(curVideo.attr( 'data-autoplay' )) : null,
                    loopVideo = null != curVideo.attr( 'data-loop' ) ? parseInt(curVideo.attr( 'data-loop' )) : null;

                if( null != id ) {
                    curPlayer = new YT.Player( this, {
                        videoId : id,
                        playerVars: { 
                            'autoplay': autoplay,
                            'loop' : loopVideo,
                            'playlist' : loopVideo ? id : '',
                            'rel' : 0
                        },
                        width : curVideo.width(),
                        height : curVideo.width()/1.7777,
                        events: {
                            'onReady': function (e) {
                                if( autoplay ){
                                    e.target.mute();   
                                }
                            }
                        }
                    });
                    videoReadyCallback( $( curPlayer.getIframe() ) );
                }
            });
        }
        //vimeo videos
        if( 0 < vimeoVideos.length ) {
            asyncloader.require( [ 'vimeonew' ], function() {
                vimeoVideos.each( function() {
                    var curVideo = $(this),
                        curPlayer = null,
                        id = !isNaN( Number( curVideo.attr( 'data-video-id' ) ) ) ? Number( curVideo.attr( 'data-video-id' ) ) : null,
                        autoplay = null != curVideo.attr( 'data-autoplay' ) ? parseInt(curVideo.attr( 'data-autoplay' )) : false,
                        loopVideo = null != curVideo.attr( 'data-loop' ) ? parseInt(curVideo.attr( 'data-loop' )) : false;
                    if( null != id ) {
                        var curPlayer = new Vimeo.Player( this, {
                            id : id,
                            autoplay : autoplay ? true : false,
                            loop : loopVideo ? true : false,
                            muted : autoplay ? true : false,
                            width : curVideo.width(),
                            height : Math.ceil(curVideo.width()/1.7777),    
                        });
                        curPlayer.ready().then( function() {
                            videoReadyCallback( curVideo.children( 'iframe' ) );
                        });
                    }
                } );
            } );
        }

        if( 0 < youtubeVideos.length ) {
            if( 'undefined' != typeof YT && 'function' == typeof YT.Player ) {
                loadYoutubeVideos();
            }else {
                $(document).on( 'YTAPIReady', loadYoutubeVideos );
            }
        }

        if( 0 < iframeEle.length ) {
            asyncloader.require( 'fitvids', function() {
                $body.fitVids({
                    customSelector: 'iframe[src*="videopress.com"]'
                });
            });
        }
        
    },
    backToTop = (function() {
        var backToTopBtn = $( '#be-themes-back-to-top' ),
            triggerBackToTop = function() {
                if( 0 < backToTopBtn.length ) {
                    if ( $win.scrollTop() > 10 ) {
                        backToTopBtn.fadeIn();
                    } else {
                        backToTopBtn.fadeOut();
                    }
                }
            };
        backToTopBtn.on( 'click', function(e){
            e.preventDefault();
            jQuery('body,html').animate({ scrollTop: 0 }, 1000 );
        });    
        if( 0 < backToTopBtn.length ) {
            $win.on( 'scroll', function() {
                triggerBackToTop();
            } );
        }
        return triggerBackToTop;
    })(),
    postsNav = function() {
        var postNav = jQuery( '.exp-posts-nav-sticky' ),
            postContent;
        if( 0 < postNav.length ) {
            postContent = jQuery( '.exp-post-single-content' );
            $body.css( 'margin-bottom', ( postNav.outerHeight() + 20 ) + 'px' );
            if( 0 < postContent.length ) {
                $win.on( 'scroll', function() {
                    var viewportBottom = $win.scrollTop() + $win.height(),
                        postContentBottom = postContent.offset().top + postContent.outerHeight();
                    if( !postNav.hasClass( 'exp-posts-nav-sticky-active' ) && 100 <  (viewportBottom - postContentBottom ) ) {
                        postNav.addClass( 'exp-posts-nav-sticky-active' );
                    }else if( postNav.hasClass( 'exp-posts-nav-sticky-active' ) && 100 > (viewportBottom - postContentBottom ) ) {
                        postNav.removeClass( 'exp-posts-nav-sticky-active' );
                    }
                });
            }else {
                postNav.addClass( 'exp-posts-nav-sticky-active' );
            }
        }
    },
    postsMeta = function() {
        var labeledMeta = $( '.exp-post-categories-labeled .exp-term' );
        if( 0 < labeledMeta.length ) {
            labeledMeta.each( function() {
                var curMeta = $(this),
                    color = curMeta.css( 'color' ),
                    bgColor = curMeta.css( 'background-color' );
                curMeta.on( 'mouseenter', function() {
                    if( '' != color || '' != bgColor ) {
                        curMeta.css( {
                            color : bgColor,
                            backgroundColor : color
                        } );
                    }
                } );
                curMeta.on( 'mouseleave', function() {
                    if( '' != color || '' != bgColor ) {
                        curMeta.css( {
                            color : color,
                            backgroundColor : bgColor
                        } );
                    }
                } );
            } );
        }
    },
    initEvents = function() {
        $win.on( 'debouncedresize', function() {
            comments();
            sticky();
        });
    },
    sticky = function() {
        var stickyElements = $( '.be-sidebar-sticky' );
        if( 0 < stickyElements.length ) {
            asyncloader.require( 'sticky-kit', function() {
                var windowWidth = $win.width(),
                    offsetTop = $body.hasClass( 'admin-bar' ) ? 32 : 0;
                stickyElements.each( function() {
                    var curEle = $(this);
                    if( 1024 < windowWidth && !curEle.data().sticky_kit ) {
                        curEle.stick_in_parent( {
                            parent : '.be-row',
                            offset_top : offsetTop
                        });
                    }else if( 1024 > windowWidth && curEle.data().sticky_kit ) {
                        curEle.trigger("sticky_kit:detach");
                    }
                });
            } );
        }
    },
    exponent_superfish = function() {
        
        asyncloader.require( [ 'superfish', 'hoverintent' ], function(){
            
            var $menu = jQuery('.exponent-menu ').children('ul');
            
            $menu.superfish({
                animation: {top: "50px", opacity: "show"},
                animationOut: {top: "45px", opacity: "hide"},
                pathLevels:3,  
                speed : "fast",
                delay: 100,
                disableHI: true,
                onBeforeShow : function() {
                    
                    if( this.parent('li').hasClass('mega-menu') ){
                        this.css('visibility','hidden');
                        this.fadeIn();
                        
                        var subMenu = this,
                            subMenuWidth = subMenu.width() ,
                            subMenuPosition = subMenu.offset().left ,
                            parentPosition = this.parent('li').offset().left;
                            if( ( jQuery(window).width() - subMenuPosition ) < subMenuWidth ){
                                var correctedPosition = subMenuWidth - ( jQuery(window).width() - 30 - subMenuPosition )
                                subMenu.css( 'left', - correctedPosition );
                                subMenu.find('.exponent-header-pointer').css( 'left', correctedPosition + 20 );
                            }
                        this.css('visibility','visible');
                        this.fadeOut();
                    }
                    else{
                        
                        var subMenuDepth = this.parents('ul').length ,
                        currentMenuItem = this.closest('li.menu-item-has-children'),
                        subMenuWidth = this.innerWidth(),
                        subMenuPositionCheck = subMenuDepth * subMenuWidth,
                        positionOffset = ( jQuery(this).innerWidth() - jQuery(this).width() ) / 2 , 
                        subMenuPosition = subMenuWidth -  positionOffset + 5;

                        if ( subMenuDepth > 1 ){                                
                            if( ( jQuery(window).width() - this.closest('li.menu-item-has-children').offset().left ) < subMenuPositionCheck ){
                                currentMenuItem.find('ul.exponent-sub-menu').css( 'right', subMenuPosition ).css( 'left' , 'auto' ).css('top', 0);
                            }else{
                                currentMenuItem.find('ul.exponent-sub-menu').css( 'left', subMenuPosition ).css( 'right' , 'auto' ).css('top', 0);
                            }
                        }
                    }
                    this.siblings('.exponent-sub-menu-indicator').addClass('menu-open');
                },
                onBeforeHide : function() {
                    this.siblings('.exponent-sub-menu-indicator').removeClass('menu-open');
                }
            });
        });		    	
    },
    exponent_menu_drop_down = function() {
        jQuery(document).on('click', '.exponent-mobile-menu-icon .line-4', function () {
            var mobileMenuIcon = jQuery(this).parent('.exponent-mobile-menu-icon');
            mobileMenuIcon.toggleClass('open');
            mobileMenuIcon.siblings('.exponent-mobile-menu ').animate({ height: 'toggle' });
        });
        jQuery(document).on('click','.exponent-mobile-menu  .exponent-sub-menu-indicator' , function() {
            jQuery(this).toggleClass('menu-open');
            jQuery(this).siblings('.exponent-sub-menu').animate({ height: 'toggle' });
        });
        jQuery(document).on('click','.exponent-mobile-menu  li.menu-item-has-children a' , function() {
            if(jQuery(this).attr('href') == '#'){
                jQuery(this).toggleClass('menu-open');
                jQuery(this).siblings('.exponent-sub-menu').animate({ height: 'toggle' });
            }
        });
    },
    animateScroll = function( target ) {
        var scrollTo,
            $header = $('#tatsu-header-wrap'),
            tatsuHeader = window.tatsuHeader,
            scrollOffset = 0;
        target = 'string' == typeof target ? jQuery( target ) : ( target instanceof $ ? target : null );
        if( null != target && 0 < target.length && target.is(':visible') ) {
            if( null != tatsuHeader ) {
                if( 960 > $win.width() && $('.tatsu-mobile-menu').is(':visible') ) {
                    tatsuHeader.closeMobileMenu();
                }
                tatsuHeader.closeSlide();
            }
            scrollTo = target.offset().top;
            //offset admin bar
            if( $body.hasClass( 'admin-bar' ) ) {
                scrollOffset+= 32;
            }
            //offset sticky header
            if( 0 < $header.length && $header.hasClass( 'sticky' ) && null != tatsuHeader ) {
                if( ( !$header.hasClass( 'smart' ) || $htmlBody.scrollTop() > scrollTo ) && scrollTo > tatsuHeader.getSmartOffset() ) {
                    scrollOffset += null != tatsuHeader ? tatsuHeader.getStickyHeaderHeight() : 0;
                }
            }
            scrollTo = Math.ceil( scrollOffset <= scrollTo ? scrollTo - scrollOffset : scrollTo );
            $htmlBody.animate({scrollTop : scrollTo}, 1000);
        }
    },
    removePageLoader = function() {
        if( 0 < $( '#be-themes-loader-container' ).length ) {
            $( '#be-themes-loader-container' ).stop().fadeOut( 400, function() {
                $(this).remove();
                $htmlBody.css('overflow-y','');
            });
        }
    },
    pageLoader = function() {
        if( 0 < $( '#be-themes-loader-container' ).length ) {
            //for page speed tests
            $htmlBody.css('overflow-y','hidden');
            $win.on( 'load', removePageLoader );
            /**
             * Handle bfcache mechanism in browsers when traversed back to a page in history
             * @source https://stackoverflow.com/questions/158319/is-there-a-cross-browser-onload-event-when-clicking-the-back-button
             */
            $win.on( 'pageshow', function() {
                $body.css({transition : '', opacity : ''});
            });
            $win.on( 'beforeunload', function() {
                $body.css({transition : 'opacity 500ms', opacity : '0'});
            });
            if( 0 < $('#tatsu-header-wrap').length && $('#tatsu-header-wrap').hasClass('transparent') ) {
                if( $body.hasClass( 'tatsu-transparent-header-pad' ) ) { 
                    removePageLoader();
                }else {
                    $doc.on( 'tatsu_transparent_header_padding_calc', removePageLoader );
                }
            }else {
                removePageLoader();
            }
        }else{
            $htmlBody.css('overflow-y','auto');
        }
    },
    localScroll = function() {
        var $hash = window.location.hash;
        if ($hash && 0 < $($hash).length ) {
            animateScroll($($hash));
        }
        $doc.on( 'click', 'a', function(e) {
            var curAnchor = $(this),
                url = curAnchor.attr( 'href' ),
                urlArray,
                targetElement,
                curLocation = window.location.href;
            if( curAnchor.hasClass('ui-tabs-anchor') || 0 < curAnchor.closest('.wc-tabs').length || '#' === url ) {
                return false;
            }
            if( $body.hasClass( 'be-sticky-sections' ) ) {
                return;
            }
            if( 'string' == typeof url ) {
                urlArray = url.split( '#' );
                targetElement = urlArray[1];
                if( 0 <= curLocation.indexOf(urlArray[0]) && 'string' == typeof targetElement ) {
                    targetElement = $('#' + targetElement);
                    if( 0 < targetElement.length && targetElement.is(':visible') ) {
                        e.preventDefault();
                        animateScroll( targetElement );
                    }
                }
            }
        });
    },
    lazyLoadBgImages = function() {
        var bgToLazyLoad = $( '.be-themes-bg-lazyload' );
        if( 0 < bgToLazyLoad.length ) {
            bgToLazyLoad.each(function() {
                var curEle = $(this),
                    curSrc = curEle.attr( 'data-src' ),
                    dummyImg;
                if( null != curSrc ) {
                    dummyImg = $(new Image());
                    dummyImg.one('load', function() {
                        curEle.addClass( 'be-themes-bg-lazyloaded' );
                        setTimeout(function() {
                            curEle.parent().find('.be-themes-bg-blur').remove();
                        }, 1000);
                    });
                    dummyImg.attr( 'src', curSrc );
                    if(dummyImg[0].complete) {
                        dummyImg.load();
                    }
                }
            });
        }
    },
    slider = function() {
        var sliders = $( '.be-slider' ),    
            initOuterArrows = function( slider ) {
                if( slider instanceof $ && 0 < slider.length && !slider.hasClass( 'be-slider-with-margin' ) && ( 100 < ( $win.width() - slider.outerWidth() ) ) ) {
                    var gutter = !isNaN( slider.attr('data-gutter') ) ? Number( slider.attr('data-gutter') )/2 : 0;
                    slider.css({
                        padding : '0 50px',
                        margin : '0 -' + ( gutter + 50 ) + 'px'
                    });
                }
            },
            getLazyLoadCount = function(slider){
                var count = 1;
                if( slider instanceof $ && 0 < slider.length ) {
                var cols = !isNaN(Number(slider.attr('data-cols'))) ? Number(slider.attr('data-cols')) : 1;
                    if( 1 < cols ) {
                        count = cols-1;
                    }
                }
                return count;
            },
            hideUnneccessaryNav = function( curSlider ) {
                var navPossible = function( slider ) {
                    var cols,
                        slidesClount;
                    if( slider instanceof $ && 0 < slider.length ) {
                        cols = !isNaN(Number(slider.attr('data-cols'))) ? Number(slider.attr('data-cols')) : 1;
                        slidesClount = slider.find('.be-slide').length;
                        if( 1024 < $win.width() ) {
                            return cols < slidesClount;
                        }else if( 767 < $win.width() ) {
                            return 2 < slidesClount;
                        }else {
                            return 1 < slidesClount;
                        }
                    }
                };
                if( !navPossible( curSlider ) ) {
                    curSlider.addClass('be-slider-hide-nav');
                }
                $win.on( 'debouncedresize', function() {
                    if( !navPossible(curSlider) ) {
                        curSlider.addClass('be-slider-hide-nav');
                    }else {
                        curSlider.removeClass('be-slider-hide-nav');
                    }
                });
            },
            equalHeightSlider = function( slider ) {
                if( slider instanceof $ && 0 < slider.length ) {
                    var maxHeight = 0,
                        slides = slider.find( '.be-slide' );
                    slides.each(function(){
                        var curSlide = $(this);
                        if( maxHeight < curSlide.height() ) {
                            maxHeight = curSlide.height();
                        }
                    });
                    slides.height( maxHeight );
                    slider.addClass( 'be-equal-height-slider' );
                }
            };
        if( 0 < sliders.length ) {
            asyncloader.require( 'flickity', function() {
                sliders.each(function() {
                    var curSlider = jQuery(this);
                    if( !curSlider.hasClass( 'flickity-enabled' ) ) {
                        if( '1' == curSlider.attr( 'data-arrows' ) || '1' == curSlider.attr( 'data-dots' ) ) {
                            hideUnneccessaryNav(curSlider);
                        }
                        if( '1' == curSlider.attr('data-arrows') && '1' == curSlider.attr('data-outer-arrows')) {
                            initOuterArrows(curSlider);
                        }
                        if( '1' == curSlider.attr( 'data-equal-height' ) ) {
                            equalHeightSlider(curSlider);
                        }
                        curSlider.flickity({
                            cellAlign : null != curSlider.attr( 'data-cell-align' ) ? curSlider.attr( 'data-cell-align' ) : 'left',
                            contain : true,
                            lazyLoad : '1' == curSlider.attr( 'data-lazy-load' ) ? getLazyLoadCount(curSlider) : false,
                            adaptiveHeight : '1' == curSlider.attr( 'data-adaptive-height' ) ? true : false,
                            pageDots : '1' == curSlider.attr('data-dots') ? true : false,
                            prevNextButtons : '1' == curSlider.attr('data-arrows') ? true : false,
                            asNavFor : null != curSlider.attr('data-as-nav-for') ? curSlider.attr('data-as-nav-for') : false,
                            autoPlay : !isNaN(Number(curSlider.attr('data-auto-play'))) ? Number(curSlider.attr('data-auto-play')) : false,
                            wrapAround : '1' == curSlider.attr('data-infinite') ? true : false,
                        });
                    }
                });
            });
        }
    },
    portfolio_fixed_page = function(){
        if( jQuery( '.fixed-sidebar-page' ).length ){
            var OverflowWrapper = jQuery( '.be-content-overflow' ),
            pageType = OverflowWrapper.hasClass( 'left-fixed-page' ) ? 'left' : 'right',
            contentWrap = jQuery( '#content-wrap' );

        jQuery( '#be-overflow-image-content-inner' ).css('margin-'+pageType,- Math.abs(contentWrap.offset().left) );
        if( jQuery( '.be-sticky-column' ).length ){
            asyncloader.require( 'sticky-kit', function() {
                var offsetTop = jQuery('body').hasClass( 'admin-bar' ) ? 32 : 0;
                jQuery( '.be-sticky-column' ).stick_in_parent( {parent:'.be-content-overflow-inner-wrap' ,offset_top:offsetTop} );
            });
        }
        }
    },
    sticky_sections = function() {
        if( $body.hasClass( 'be-sticky-sections' ) && !$body.hasClass( 'tatsu-frame' ) ) {
            asyncloader.require( 'stickysections', function() {
                var stickyScrollType = $( '.be-sections-wrap' ).attr( 'data-sticky-scroll' ),
                    globalSections = $('.tatsu-global-section'),
                    defaultHeaderHeight = $( '#tatsu-header-container' ).outerHeight() + ( $( '#wpadminbar' ).length ? $( '#wpadminbar' ).height() : 0 ),
                    transparentHeaderDynamicColorChange = function() {
                        var curSection = jQuery( this ),
                            headerInnerWrap = jQuery( '#tatsu-header-wrap' ),
                            headerScheme,
                            headerSchemeFromSection = curSection.attr( 'data-headerscheme' );
                        if( headerInnerWrap.hasClass( 'transparent' ) && null != headerSchemeFromSection ) {
                            headerScheme = headerInnerWrap.hasClass( 'dark' ) ? 'dark' : 'light';
                            headerSchemeFromSection = headerSchemeFromSection.replace( 'background--', '' );
                            headerInnerWrap.removeClass( headerSchemeFromSection );
                            headerInnerWrap.addClass( 'dark' == headerSchemeFromSection ? 'light' : 'dark' );
                        }
                    },
                    navScroll = function() {
                        $doc.on( 'click', 'a', function( e ) {
                            var curAnchor = $(this),
                                url = curAnchor.attr( 'href' ),
                                urlArray,
                                targetElement,
                                secIndex,
                                curLocation = window.location.href;
                            if( 'string' == typeof url ) {
                                urlArray = url.split( '#' );
                                targetElement = urlArray[1];
                                if( 0 <= curLocation.indexOf(urlArray[0]) && 'string' == typeof targetElement ) {
                                    targetElement = $('#' + targetElement);
                                    if( 0 < targetElement.length && targetElement.is(':visible') ) {
                                        e.preventDefault();
                                        secIndex = stickySections.getStickyIndex( targetElement[0] );
                                        if( -1 < secIndex && secIndex < jQuery( '.sticky-section' ).length ) {
                                            if( $body.scrollTop() === 0 ) {
                                                $body.scrollTop(1);
                                            }
                                            stickySections.moveTo( secIndex );
                                        }
                                    }
                                }
                            }
                        });
                    },
                    triggerTatsuAnimation = function() {
                        if( null != tatsu ) {
                            tatsu.cssAnimate( false, '', $(this) );
                            if( $(this).find( '.tatsu-an' ).length ) {
                                tatsu.animatedNumbers();
                            }
                        }
                    },
                    stickySectionDotsDynamicColorChange = function() {
                        var curSection = $( this ),
                            dotsNav = $( '#sticky-dots-navigation' ),
                            headerScheme,
                            headerSchemeFromSection = curSection.attr( 'data-headerscheme' );
                        if( 0 < dotsNav.length ) {
                            headerScheme = dotsNav.attr( 'data-headerscheme' );
                            if( headerScheme != headerSchemeFromSection ) {
                                dotsNav.find( 'span' ).css( 'transition', 'none' );
                                dotsNav.removeClass( 'background--dark' == headerSchemeFromSection ? 'background--light' : 'background--dark' );
                                dotsNav.find( 'span' ).css( 'transition', 'background 700ms cubic-bezier(0.645, 0.045, 0.355, 1)' );
                                dotsNav.addClass( headerSchemeFromSection );
                                dotsNav.attr( 'data-headerscheme', headerSchemeFromSection );
                            }
                        }
                    },
                    setCurrentActiveMenu = function() {
                        var menuItems = $( 'li.menu-item' ),
                            curActiveSection = $( this );
                        if( curActiveSection.length && ( ( defaultHeaderHeight + $win.scrollTop() ) >= curActiveSection.offset().top ) ) {
                            menuItems.removeClass( 'current-menu-item' );
                            menuItems.find('a[href$="#'+ curActiveSection.attr( 'id' ) +'"]').closest('li.menu-item').addClass('current-menu-item current-section');
                        }
                    },
                    initCallBack = function( activeSection ) {
                        stickySections.updateLayout();
                        stickySectionDotsDynamicColorChange.call(activeSection);
                        setCurrentActiveMenu.call(activeSection);
                        triggerTatsuAnimation.call(activeSection);
                        navScroll();
                    },
                    enableOverlay = jQuery( '.be-sections-wrap' ).attr( 'data-sticky-overlay' ),
                    stickyOptions = {
                        autoScroll : 'auto_scroll' == stickyScrollType ? true : false,
                        fixedParent : '#be-sticky-section-fixed-wrap',
                        scrollCallback : function( secIndex ) {
                            triggerTatsuAnimation.call(this);
                        },
                        scrollingSpeed : 1200,
                        overlay : 0 == enableOverlay ? false : true,
                        fullScreenOffset : [ '#wpadminbar', '#tatsu-header-container' ],
                        dots : true,
                        footer : [ '.tatsu-global-section-bottom', '#tatsu-footer-container' ],
                        navigationPosition : 'right',
                        afterLoad : function() {
                            triggerTatsuAnimation.call(this);
                            transparentHeaderDynamicColorChange.call(this, arguments);
                            stickySectionDotsDynamicColorChange.call(this);
                            setCurrentActiveMenu.call(this);
                        }
                    };
                if( 0 < globalSections.length ) {
                    $('.be-sections-wrap').prepend( jQuery('.tatsu-global-section-top') ).append( jQuery('.tatsu-global-section-penultimate') );
                }
                /***  Auto/Normal scroll Touch screen : START *****/
                var isTouchScreen = (('ontouchstart' in window) || (navigator.maxTouchPoints > 0) || (navigator.msMaxTouchPoints > 0));
                
                if( !isTouchScreen ) {
                    stickySections.initialize( '.be-sections-wrap', stickyOptions , initCallBack );
                }
                $win.on( 'resize', function() {
                    isTouchScreen = (('ontouchstart' in window) || (navigator.maxTouchPoints > 0) || (navigator.msMaxTouchPoints > 0));
                    if( !isTouchScreen && !$body.hasClass( 'sticky-enabled' ) ) {
                        stickySections.initialize( '.be-sections-wrap', stickyOptions , initCallBack );
                    }else if( isTouchScreen && $body.hasClass( 'sticky-enabled' ) ) {
                        stickySections.destroy();
                    }
                } )
                /***  Auto/Normal scroll Touch screen : END *****/

                // if( 967 < jQuery( window ).width() ) {
                //     stickySections.initialize( '.be-sections-wrap', stickyOptions , initCallBack );
                // }
                // $win.on( 'resize', function() {
                //     if( 967 < $win.width() && !$body.hasClass( 'sticky-enabled' ) ) {
                //         stickySections.initialize( '.be-sections-wrap', stickyOptions , initCallBack );
                //     }else if( 968 > $win.width() && $body.hasClass( 'sticky-enabled' ) ) {
                //         stickySections.destroy();
                //     }
                // } )
            } );
        }
    },
    init = function() {
        lazyLoadBgImages();
        localScroll();
        backToTop();
        pageLoader();
        video();
        grid();
        postsMeta();
        comments();
        expose();
        initEvents();
        postsNav();
        sticky();
        slider();
        exponent_superfish();
        exponent_menu_drop_down();
        portfolio_fixed_page();
        sticky_sections();
    };
    jQuery(function() {
        init();
        removePageLoader();
    });
    registerDependencies();

})( jQuery );