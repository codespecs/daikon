#!/bin/sh
# ==================================================================
# POV-Ray 3.6 - Unix source version - KDE install script
# ==================================================================
# written July 2003 - March 2004 by Christoph Hormann
# Based on parts of the Linux binary version install script
# This file is part of POV-Ray and subject to the POV-Ray licence
# see POVLEGAL.DOC for details.
# ==================================================================


# @@KDE_BEGIN@@

VERSION=3.6
VER_DIR=povray-$VERSION
DEFAULT_DIR=/usr/local
SYSCONFDIR=$DEFAULT_DIR/etc

# ==================================================================
#    Add read+write path to user povray.conf file
# ==================================================================

add_readwrite_conf()
{
  DIR_NAME=$1
  CONF_FILE="$HOME/.povray/$VERSION/povray.conf"

  echo "  checking conf file $CONF_FILE"

  if [ ! -f "$CONF_FILE" ] ; then
		cp -f "$SYSCONFDIR/povray.conf" "$CONF_FILE"
  fi

  if [ -w "$CONF_FILE" ] ; then

		if grep -E -i "$DIR_NAME" "$CONF_FILE" > /dev/null ; then
			echo "    - file does not need to be modified"
		else
			echo "    - adding new read+write path"

			cp -f "$CONF_FILE" "$CONF_FILE.bak"

			grep -B 1000 -E -i "^\[Permitted Paths\]" "$CONF_FILE.bak" > "$CONF_FILE" 2> /dev/null

			echo ";--- Lines added by POV-Ray $VERSION install script ---" >> "$CONF_FILE"
			echo "read+write* =  \"$DIR_NAME\"" >> "$CONF_FILE"
			echo ";---------------------------------------------------" >> "$CONF_FILE"

			grep -A 1000 -E -i "^\[Permitted Paths\]" "$CONF_FILE.bak" | sed "/^\[Permitted Paths\]/d" >> "$CONF_FILE" 2> /dev/null

			rm -f "$CONF_FILE.bak"

		fi

  else
    echo "Error: could not modify povray.conf"
  fi
}

# ==================================================================
#    Add read path to user povray.conf file
# ==================================================================

add_read_conf()
{
  DIR_NAME=$1
  CONF_FILE="$HOME/.povray/$VERSION/povray.conf"

  echo "  checking conf file $CONF_FILE"

  if [ ! -f "$CONF_FILE" ] ; then
		cp -f "$SYSCONFDIR/povray.conf" "$CONF_FILE"
  fi

  if [ -w "$CONF_FILE" ] ; then

		if grep -E -i "$DIR_NAME" "$CONF_FILE" > /dev/null ; then
			echo "    - file does not need to be modified"
		else
			echo "    - adding new read path"

			cp -f "$CONF_FILE" "$CONF_FILE.bak"

			grep -B 1000 -E -i "^\[Permitted Paths\]" "$CONF_FILE.bak" > "$CONF_FILE" 2> /dev/null

			echo ";--- Lines added by POV-Ray $VERSION install script ---" >> "$CONF_FILE"
			echo "read* =  \"$DIR_NAME\"" >> "$CONF_FILE"
			echo ";---------------------------------------------------" >> "$CONF_FILE"

			grep -A 1000 -E -i "^\[Permitted Paths\]" "$CONF_FILE.bak" | sed "/^\[Permitted Paths\]/d" >> "$CONF_FILE" 2> /dev/null

			rm -f "$CONF_FILE.bak"

		fi

  else
    echo "Error: could not modify povray.conf"
  fi
}

# ==================================================================
#    Determine installation dir from Library_path settings in ini
# ==================================================================

install_dir()
{
  if [ -z "$POVINI" ] ; then
    test -f "$SYSCONFDIR/povray.ini" && POVINI="$SYSCONFDIR/povray.ini"
    test -f "$HOME/.povrayrc" && POVINI="$HOME/.povrayrc"
    test -f "$SYSCONFDIR/povray/$VERSION/povray.ini" && POVINI="$SYSCONFDIR/povray/$VERSION/povray.ini"
    test -f "$HOME/.povray/$VERSION/povray.ini" && POVINI="$HOME/.povray/$VERSION/povray.ini"
  fi

  if [ ! -z "$POVINI" ] ; then
    # this is not a completely failsafe method but it should work in most cases
    INSTALL_DIR=`grep -E -i "^library_path=.*share/$VER_DIR" "$POVINI" | head -n 1 | sed "s?[^=]*=\"*??;s?/share/$VER_DIR.*??"`
    echo "$INSTALL_DIR"
  fi
}

# ==================================================================
#    Add file name to install log
# ==================================================================

log_install()
{
  if [ -w "$DEFAULT_DIR/share/$VER_DIR/" ] ; then
		LOG_NAME="$DEFAULT_DIR/share/$VER_DIR/install.log"
	else
    if [ -w "$HOME/.povray/$VERSION/" ] ; then
			LOG_NAME="$HOME/.povray/$VERSION/install.log"
		else
		  return 0
		fi
  fi

  if [ -z "$1$2" ] ; then
		rm -f "$LOG_NAME"
	fi

	if [ ! -f "$LOG_NAME" ] ; then
	  echo "# POV-Ray version $VERSION install log" > "$LOG_NAME"
	  echo "# started `date`" >> "$LOG_NAME"
	fi

  if [ "$1" = "B" ] ; then
		FILE_NAME=`echo "$2" | sed "s? ?%?g"`
    FILE_NAME="$FILE_NAME $3"
  else 
		FILE_NAME=`echo "$2" | sed "s? ?%?g"`
	fi

	echo "$1 $FILE_NAME" >> "$LOG_NAME"
}

# ==================================================================
#    install KDE panel entries
# ==================================================================

kde_install()
{
  if [ -z "$1" ] ; then
    INSTALL_DIR=`install_dir`
  else
    INSTALL_DIR="$1"
  fi

  if [ -z "$INSTALL_DIR" ] ; then
    echo "------------------------------------------------------"
    echo "KDE integration NOT successful."
    echo "The directory where POV-Ray is installed could not be" 
    echo "determined.  Make sure POV-Ray is correctly installed"
    echo "on this computer"
    echo "------------------------------------------------------"
    return 0
  fi

  echo "------------------------------------------------------"
  echo "installing KDE integration for user '$USER'..."

  if [ -z "$KDEHOME" ] ; then 
    if [ -d "$HOME/.kde" ] ; then 
      KDEHOME="$HOME/.kde"
    else
      echo "could not determine user KDEHOME directory."
      echo "make sure KDE is correctly installed"
      return 0
    fi
  else
    if [ ! -d "$KDEHOME" ] ; then
      echo "user KDEHOME directory ($KDEHOME) does not exist"
      return 0
    fi
  fi

  if [ ! -w "$KDEHOME" ] ; then
    echo "no write permission for user KDEHOME directory ($KDEHOME)"
    return 0
  fi

  test -d "$KDEHOME/share" || mkdir "$KDEHOME/share"

  if [ -d "$INSTALL_DIR/share/$VER_DIR/icons" ] ; then

    echo "  copying POV-Ray icons..."

    test -d "$KDEHOME/share/icons" || mkdir "$KDEHOME/share/icons"

		ICON_SETS="hicolor crystalsvg slick"

		for ICON_SET in $ICON_SETS ; do

			case $ICON_SET in
				"hicolor")
					ICON_SET_INTERN="classic"
					;;
				"crystalsvg")
					ICON_SET_INTERN="crystal"
					;;
				"slick")
					ICON_SET_INTERN="slick"
					;;
			esac

			test -d "$KDEHOME/share/icons/$ICON_SET" || mkdir "$KDEHOME/share/icons/$ICON_SET"

			ICON_SIZES="16 32 48 64"

			for ICON_SIZE in $ICON_SIZES ; do

				test -d "$KDEHOME/share/icons/$ICON_SET/${ICON_SIZE}x${ICON_SIZE}" || mkdir "$KDEHOME/share/icons/$ICON_SET/${ICON_SIZE}x${ICON_SIZE}"
				test -d "$KDEHOME/share/icons/$ICON_SET/${ICON_SIZE}x${ICON_SIZE}/mimetypes" || mkdir "$KDEHOME/share/icons/$ICON_SET/${ICON_SIZE}x${ICON_SIZE}/mimetypes"
				cp -f "$INSTALL_DIR/share/$VER_DIR/icons/file_pov_classic_16.png" "$KDEHOME/share/icons/hicolor/16x16/mimetypes/povsdl_pov.png"

				if [ "$ICON_SET" = "hicolor" ] ; then
					test -d "$KDEHOME/share/icons/$ICON_SET/${ICON_SIZE}x${ICON_SIZE}/apps" || mkdir "$KDEHOME/share/icons/$ICON_SET/${ICON_SIZE}x${ICON_SIZE}/apps"
					cp -f "$INSTALL_DIR/share/$VER_DIR/icons/povray_${ICON_SIZE}.png" "$KDEHOME/share/icons/hicolor/${ICON_SIZE}x${ICON_SIZE}/apps/povray.png"
					log_install "F" "$KDEHOME/share/icons/hicolor/${ICON_SIZE}x${ICON_SIZE}/apps/povray.png"
				fi

				cp -f "$INSTALL_DIR/share/$VER_DIR/icons/file_pov_${ICON_SET_INTERN}_${ICON_SIZE}.png" "$KDEHOME/share/icons/$ICON_SET/${ICON_SIZE}x${ICON_SIZE}/mimetypes/povsdl_pov.png"
				cp -f "$INSTALL_DIR/share/$VER_DIR/icons/file_inc_${ICON_SET_INTERN}_${ICON_SIZE}.png" "$KDEHOME/share/icons/$ICON_SET/${ICON_SIZE}x${ICON_SIZE}/mimetypes/povsdl_inc.png"
				log_install "F" "$KDEHOME/share/icons/$ICON_SET/${ICON_SIZE}x${ICON_SIZE}/mimetypes/povsdl_pov.png"
				log_install "F" "$KDEHOME/share/icons/$ICON_SET/${ICON_SIZE}x${ICON_SIZE}/mimetypes/povsdl_inc.png"

			done

		done

    ICON_FILE="povray.png"

    echo "  generating POV-Ray file types..."

    test -d "$KDEHOME/share/mimelnk/text" || mkdir -p "$KDEHOME/share/mimelnk/text"

    echo "[Desktop Entry]
Comment=POV-Ray script file
Icon=povsdl_pov
Type=MimeType
MimeType=text/x-povray-script
Patterns=*.pov;*.POV;
" > "$KDEHOME/share/mimelnk/text/x-povray-script.desktop"

		log_install "F" "$KDEHOME/share/mimelnk/text/x-povray-script.desktop"

    echo "[Desktop Entry]
Comment=POV-Ray include file
Icon=povsdl_inc
Type=MimeType
MimeType=text/x-povray-include
Patterns=*.inc;*.INC;
" > "$KDEHOME/share/mimelnk/text/x-povray-include.desktop"

		log_install "F" "$KDEHOME/share/mimelnk/text/x-povray-include.desktop"

  else

    echo "Could not find required files, make sure POV-Ray $VERSION is correctly installed"
    echo ""

  fi

  echo "  installing main POV-Ray $VERSION submenu..."

  if [ ! -d "$KDEHOME/share/applnk" ] ; then 
		mkdir -p "$KDEHOME/share/applnk"
    KDE_PANEL_DIR="$KDEHOME/share/applnk/$VER_DIR"
  else
    KDE_PANEL_DIR="$KDEHOME/share/applnk/$VER_DIR"
  fi

  if [ -d "$KDE_PANEL_DIR" ] ; then 
    rm -rf $KDE_PANEL_DIR/*
  else
    mkdir "$KDE_PANEL_DIR"
  fi

	log_install "F" "$KDE_PANEL_DIR"

  echo "[Desktop Entry]
Name=POV-Ray $VERSION
Icon=povray
" > "$KDE_PANEL_DIR/.directory"

  echo "  installing ini file link..."

  if [ -f "$HOME/.povray/$VERSION/povray.ini" ] ; then
    POVINI="$HOME/.povray/$VERSION/povray.ini"
    echo "[Desktop Entry]
Type=Application
Exec=kwrite $POVINI
Icon=txt
Name=edit user povray ini file (~/.povray/$VERSION/povray.ini)
" > "$KDE_PANEL_DIR/ini.desktop"
  else
    POVINI="$SYSCONFDIR/povray/$VERSION/povray.ini"
    echo "[Desktop Entry]
Type=Application
Exec=kdesu kwrite $POVINI
Icon=txt
Name=edit global povray.ini file
" > "$KDE_PANEL_DIR/ini.desktop"
  fi

  echo "  installing configuration file link..."

  if [ -f "$HOME/.povray/$VERSION/povray.conf" ] ; then
    POVCONF="$HOME/.povray/$VERSION/povray.conf"
    echo "[Desktop Entry]
Type=Application
Exec=kwrite $POVCONF
Icon=txt
Name=edit IO-restrictions configuration file (~/.povray/$VERSION/povray.conf)
" > "$KDE_PANEL_DIR/conf_user.desktop"
  fi

  if [ -f "$SYSCONFDIR/povray/$VERSION/povray.conf" ] ; then
    POVCONF="$SYSCONFDIR/povray/$VERSION/povray.conf"
    echo "[Desktop Entry]
Type=Application
Exec=kdesu kwrite $POVCONF
Icon=txt
Name=edit global IO-restrictions configuration file
" > "$KDE_PANEL_DIR/conf_sys.desktop"
  fi

  echo "  installing documentation link..."

  echo "[Desktop Entry]
Type=Application
Exec=konqueror $INSTALL_DIR/share/doc/$VER_DIR/html/index.html
Icon=html
Name=Documentation
" > "$KDE_PANEL_DIR/docu.desktop"

  echo "[Desktop Entry]
Type=Application
Exec=konqueror $INSTALL_DIR/share/doc/$VER_DIR/html/povlegal.html
Icon=html
Name=The POV-Ray licence (POVLEGAL.DOC)
" > "$KDE_PANEL_DIR/povlegal.desktop"

  echo "[Desktop Entry]
Type=Application
Exec=povray -benchmark ; read
Icon=exec
Name=run benchmark
Terminal=1
" > "$KDE_PANEL_DIR/benchmark.desktop"

  if [ -d "$INSTALL_DIR/share/$VER_DIR/scripts/" ] ; then

    echo "  installing sample scene render links..."

    if [ -w "$INSTALL_DIR/share/$VER_DIR" ] ; then 
      SAMPLE_RESULTS_DIR="$INSTALL_DIR/share/$VER_DIR"
    else
      SAMPLE_RESULTS_DIR="$HOME/$VER_DIR"
      test -d "$SAMPLE_RESULTS_DIR" || mkdir "$SAMPLE_RESULTS_DIR"
      echo "This directory is generated by the POV-Ray $VERSION install script 
to contain the sample scene renders generated by the corresponding
entries in the KDE panel menu." > "$SAMPLE_RESULTS_DIR/README" 
    fi
    
    test -d "$SAMPLE_RESULTS_DIR/samples" || mkdir "$SAMPLE_RESULTS_DIR/samples"
    test -d "$SAMPLE_RESULTS_DIR/portfolio" || mkdir "$SAMPLE_RESULTS_DIR/portfolio"

    echo "Here you can find the rendered sample animations after running the sample animations render script" > "$SAMPLE_RESULTS_DIR/samples/animations.html"    
    echo "Here you can find the rendered sample scenes after running the sample scenes render script" > "$SAMPLE_RESULTS_DIR/samples/stills.html"   
    echo "Here you can find the portfolio after running the portfolio render script" > "$SAMPLE_RESULTS_DIR/portfolio/index.html"   

    if [ -d "$KDE_PANEL_DIR/samples" ] ; then 
      rm -rf $KDE_PANEL_DIR/samples*
    else
      mkdir "$KDE_PANEL_DIR/samples"
    fi

    echo "[Desktop Entry]
Name=Sample scenes
Icon=folder
" > "$KDE_PANEL_DIR/samples/.directory"

    echo "[Desktop Entry]
Type=Application
Exec=$INSTALL_DIR/share/$VER_DIR/scripts/allscene.sh -o $SAMPLE_RESULTS_DIR/samples -h $SAMPLE_RESULTS_DIR/samples/stills.html
Icon=exec
Name=render sample scenes (stills)
Terminal=1
" > "$KDE_PANEL_DIR/samples/render_stills.desktop"

    echo "[Desktop Entry]
Type=Application
Exec=$INSTALL_DIR/share/$VER_DIR/scripts/allanim.sh -o $SAMPLE_RESULTS_DIR/samples -h $SAMPLE_RESULTS_DIR/samples/animations.html
Icon=exec
Name=render sample scenes (animations)
Terminal=1
" > "$KDE_PANEL_DIR/samples/render_animations.desktop"

    echo "[Desktop Entry]
Type=Application
Exec=$INSTALL_DIR/share/$VER_DIR/scripts/portfolio.sh -o $SAMPLE_RESULTS_DIR/portfolio
Icon=exec
Name=render portfolio
Terminal=1
" > "$KDE_PANEL_DIR/samples/render_portfolio.desktop"

    echo "[Desktop Entry]
Type=Application
Exec=konqueror $SAMPLE_RESULTS_DIR/portfolio/index.html
Icon=html
Name=View portfolio
" > "$KDE_PANEL_DIR/samples/view_portfolio.desktop"

    echo "[Desktop Entry]
Type=Application
Exec=konqueror $SAMPLE_RESULTS_DIR/samples/animations.html
Icon=imagegallery
Name=Sample scene gallery (animations)
" > "$KDE_PANEL_DIR/samples/animations.desktop"

    echo "[Desktop Entry]
Type=Application
Exec=konqueror $SAMPLE_RESULTS_DIR/samples/stills.html
Icon=imagegallery
Name=Sample scene gallery (stills)
" > "$KDE_PANEL_DIR/samples/stills.desktop"

    echo "  modifying povray.conf..."
		add_readwrite_conf "$SAMPLE_RESULTS_DIR"

  else

    echo "Could not find required files, make sure POV-Ray $VERSION is correctly installed"
    echo ""

  fi

  # needs an extra invitation
	if [ -d "$KDEHOME/share/applnk-redhat" ] ; then 
    KDE_RH_PANEL_DIR="$KDEHOME/share/applnk-redhat/$VER_DIR"
		if [ -L "$KDE_RH_PANEL_DIR" ] ; then 
		  rm "$KDE_RH_PANEL_DIR"
		fi
		if [ -d "$KDE_RH_PANEL_DIR" ] ; then 
		  rm -rf "$KDE_RH_PANEL_DIR"
		fi
		ln -s "$KDE_PANEL_DIR" "$KDE_RH_PANEL_DIR"
		log_install "F" "$KDE_RH_PANEL_DIR"
  fi

  echo "Finished installing KDE panel entries"
  echo "------------------------------------------------------"
  echo ""

  return 1
}

# @@KDE_END@@


kde_install

