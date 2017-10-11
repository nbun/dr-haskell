#!/bin/bash


function main() {
	echo "============================================"
	echo "This is an installation script for DrHaskell"
	echo "============================================"
	echo
	echo -n "Checking required dependencies... "

	check_dependencies
}

function check_dependencies() {
	local missings=""
	missings="$missings `check_ghc`"
	missings="$missings `check_cabal`"
	if [[ $missings =~ ^\ +$ ]]; then
		echo "Okay."
		return 0
	else
		echo "The following packages seem to be missing:$missings"
		show_install_command "$missings"
		return 1
	fi
}

function check_ghc() {
	if ! which ghca >/dev/null 2>/dev/null; then
		echo -n "ghc"
	fi
}

function check_cabal() {
	if ! which cabala >/dev/null 2>/dev/null; then
		echo -n "cabal-install"
	fi
}

function show_install_command() {
	echo -n "Probing your operating system... "
	#local os=`determine_os`
	local os=Unknown
	echo "Found $os"
	local translator=`get_os_translator $os`
	$translator "$@"
	echo
	echo
	echo "Aborting now"
}

function determine_os() {
	# found at https://unix.stackexchange.com/questions/6345/how-can-i-get-distribution-name-and-version-number-in-a-simple-shell-script
	if [ -f /etc/os-release ]; then
		# freedesktop.org and systemd
		. /etc/os-release
		local OS=$NAME
	elif type lsb_release >/dev/null 2>&1; then
		# linuxbase.org
		local OS=$(lsb_release -si)
	elif [ -f /etc/lsb-release ]; then
		# For some versions of Debian/Ubuntu without lsb_release command
		. /etc/lsb-release
		local OS=$DISTRIB_ID
	elif [ -f /etc/debian_version ]; then
		# Older Debian/Ubuntu/etc.
		local OS=Debian
	elif [ -f /etc/SuSe-release ]; then
		local OS=Suse
	elif [ -f /etc/redhat-release ]; then
		local OS=Redhat
	else
		# Fall back to uname, e.g. "Linux <version>", also works for BSD, etc.
		local OS=$(uname -s)
	fi

	case $OS in
	Debian)
		;;
	Ubuntu)
		;;
	Arch)
		;;
	Fedora)
		;;
	Redhat)
		;;
	Suse)
		;;
	Gentoo)
		;;
	*)
		OS=Unknown
		;;
	esac

	echo $OS
}

function get_os_translator() {
	echo "print_install_command_$1"
}

function print_install_header() {
	echo
	echo "Run this command to install the dependencies:"
	echo
	echo
}

function print_install_command_Gentoo() {
	print_install_header
	echo "  sudo emerge -v$@"
}

function print_install_command_Debian() {
	print_install_header
	echo "  sudo apt install$@"
}

function print_install_command_Ubuntu() {
	print_install_command_Debian "$@"
}

function print_install_command_Arch() {
	print_install_header
	echo "  sudo pacman -S$@"
}

function print_install_command_Redhat() {
	print_install_header
	echo "  sudo dnf install$@"
}

function print_install_command_Fedora() {
	print_install_command_Redhat "$@"
}

function print_install_command_Suse() {
	print_install_header
	echo "  sudo zypper install$@"
}

function print_install_command_Unknown() {
	echo
	echo
	echo
	echo "  Unfortunately, we do not know your operating system and cannot"
	echo "  tell you, how to install dependencies."
	echo
	echo "  However, you need these packages, in order to proceed:$@"
	echo "  Note, that they may be called differently on your system."
}

main