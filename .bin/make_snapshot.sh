#!/bin/bash
# ---------------------------------------------------------------------
#
# Make rotating full system snapshots. Some notes on the setup:
#
# Firstly, the partition of my external hard drive that I use for
# backups is mounted at /root/backup using the following entry in the
# /etc/fstab file:
#
# UUID=93d50ce0-c86a-41bf-b610-b28654309b0d /root/backup/ ext4 nosuid,nodev,nofail,noauto 0 0
#
# It can be mounted graphically through Linux Mint 18 MATE's Disks
# utility.  When mounted, the /root/backup directory is read-write for
# the root user.  The backup partition has a directory layout such
# that /root/backup/backups/acer_laptop/ is where the full system
# backups are stored.
#
# To safely view the drive, the regular user should see it as
# read-only (i.e. cannot delete anything by accident). This is
# achieved using NFS share. To install NFS, run:
#
# apt-get install nfs-kernel-server
# 
# After installation, in /etc/exports I added the following line as
# suggested by
# (http://www.mikerubel.org/computers/rsync_snapshots/#ReadOnly):
#
# /root/backup localhost(secure,ro,no_root_squash)
#
# Then, you can run `mkdir /mnt/backup` and to view read-only the
# backup drive run:
#
# sudo mount localhost:/root/backup /mnt/backup
#
# Browsing to /mnt/backup, you have a read-only view of the entire
# backup drive as a normal (i.e. non-root) user. To unmount the
# read-only share, run `sudo umount /mnt/backup`.
#
# Usage instructions:
#
# This script is quite general. You just have to specify the backup
# folder prefix [hourly/daily/weekly/monthly] and, optionally, the
# desired snapshot history for this prefix (up to 99, meaning that a
# 100 snapshot history is kept). Then, use cron and/or anacron by
# editing:
#
# sudo crontab -e # setup cron
# sudo emacs /etc/anacrontab # setup anacron
#
# This will allow you to setup the desired backup scheme. I have for
# example:
#
# ######## cron
#   # full-system backup
#   # NB: mail requires to have installed
#   #  sudo apt-get install ssmtp
#   #  sudo apt-get install mailutils
#   # hourly backup: every 4th hour on the hour, keep 6 snapshots
#   0 */4 * * * /usr/bin/flock -w 10800 /var/lock/my_cron_backup.lock /home/danylo/.bin/make_snapshot.sh -m -p hourly -d 6 2>&1 | mail -s "Cron job: hourly backup" "danylo.malyuta@gmail.com"
#   # daily, weekly and monthly backups done by anacrontab (see /etc/anacrontab and /var/spool/anacron/)
#
# ######## anacron
#   # backups
#   # daily with history of 6
#   1	5	backup.daily	/usr/bin/flock -w 10800 /var/lock/my_cron_backup.lock /home/danylo/.bin/make_snapshot.sh -p daily -d 6 2>&1 | mail -s "Cron job: daily backup" "danylo.malyuta@gmail.com"
#   # weekly backup with history of 4
#   7	25	backup.weekly	/usr/bin/flock -w 10800 /var/lock/my_cron_backup.lock /home/danylo/.bin/make_snapshot.sh -p weekly -d 4 2>&1 | mail -s "Cron job: weekly backup" "danylo.malyuta@gmail.com"
#   # monthly backup with history of 12
#   30	45	backup.monthly	/usr/bin/flock -w 10800 /var/lock/my_cron_backup.lock /home/danylo/.bin/make_snapshot.sh -p monthly -d 12 2>&1 | mail -s "Cron job: monthly backup" "danylo.malyuta@gmail.com"
#
# The advantage of anacron is that it ensures that daily, weekly and
# monthly backups are made as soon as the computer is turned on after
# being turned off.
#
# ---------------------------------------------------------------------

########## some main variables

BACKUP_DIR="/root/backup/backups/acer_laptop/" # directory storing all backups
SOURCE_DIR="/" # directory to back up
ABORT_MSG="Will not proceed with backup. Aborting..."
DATE=$(date +"%d%m%Y_T%H%M%S")
LEVELS=("hourly" "daily" "weekly" "monthly")
CRITICAL_SPACE_LEFT=50 # GB, if less than this left on backup media then do not backup

########## exit statuses

SUCCESS=0
BACKUP_DIR_NOT_FOUND=1
UNKNOWN_FLAG=2
EXCESS_DEPTH=3
BAD_BACKUP_DIR=4
RSYNC_FAIL=5
NOT_ENOUGH_SPACE=6

########## function return values

PASS=0
FAIL=1

########## functions

echoerr()
{
    # output to STDERR (standard error stream)
    echo "$@" 1>&2
}

isdir()
{
    # check if is a directory
    local DIRECTORY="$1"
    if [[ -d "$DIRECTORY" ]] && [[ -n "$DIRECTORY" ]]; then
	return $PASS # true (is a directory)
    else
	return $FAIL # false (not a directory)
    fi
}

check_format()
{
    # check that string has correct format
    local STRING="$1"
    local FORMAT="$2"
    if [[ "$STRING" =~ $FORMAT ]]; then
	return $PASS
    else
	return $FAIL
    fi
}

check_range()
{
    # check that $LOWER <= $NUMBER <= $UPPER
    local NUMBER=$((10#$1))
    local LOWER=$((10#$2))
    local UPPER=$((10#$3))
    if [[ $NUMBER -ge $LOWER ]] && [[ $NUMBER -le $UPPER ]]; then
	return $PASS
    else
	return $FAIL
    fi
}

number_check()
{
    # check that number has correct format and range
    # check correct format
    local STRING="$1"
    local FORMAT="$2"
    if ! check_format "$STRING" "$FORMAT"; then
	return $FAIL
    fi
    # check correct range
    local NUMBER=$((10#$1))
    local LOWER=$((10#$3))
    local UPPER=$((10#$4))
    if ! check_range $NUMBER $LOWER $UPPER; then
	return $FAIL
    fi  
}

string_check()
{
    # check that string has correct format
    local STRING="$1"
    local FORMAT="$2"
    if ! check_format "$STRING" "$FORMAT"; then
	return $FAIL
    fi
}

check_name_format()
{
    # check that $NAME is in the format: NN.STRING_ddmmYYYY_THHMMSS
    # extract parts of $NAME
    local NAME="$1"
    local NN; NN=$(echo "$NAME" | cut -d . -f 1)
    local STRING_ddmmYYYY_THHMMSS; STRING_ddmmYYYY_THHMMSS=$(echo "$NAME" | cut -d . -f 2)
    local STRING; STRING=$(echo "$STRING_ddmmYYYY_THHMMSS" | cut -d _ -f 1)
    local ddmmYYYY; ddmmYYYY=$(echo "$STRING_ddmmYYYY_THHMMSS" | cut -d _ -f 2)
    local THHMMSS; THHMMSS=$(echo "$STRING_ddmmYYYY_THHMMSS" | cut -d _ -f 3)
    local HHMMSS=${THHMMSS:1}
    local dd=${ddmmYYYY:0:2}
    local mm=${ddmmYYYY:2:2}
    local YYYY=${ddmmYYYY:4:4}
    local HH=${HHMMSS:0:2}
    local MM=${HHMMSS:2:2}
    local SS=${HHMMSS:4:2}
    # check format
    # hour/minute/second should be prepended with a T
    if ! string_check "${THHMMSS:0:1}" "T"; then 
	return $FAIL
    fi
    # check NN is a double-digit number between 00 and 99 (zero padded)
    if ! number_check "$NN" "^[0-9]{2}$" 0 99; then
	return $FAIL
    fi
    # check STRING is a pure lowercase string
    if ! string_check "$STRING" "^[a-z]+$"; then
	return $FAIL
    fi
    # check ddmmYYYY is a set of 8 numbers
    if ! string_check "$ddmmYYYY" "^[0-9]{8}$"; then
	return $FAIL
    fi
    # check dd is a day between 01 and 31 (zero padded)
    if ! number_check "$dd" "^[0-9]{2}$" 1 31; then
	return $FAIL
    fi
    # check mm is a month between 01 and 12 (zero padded)
    if ! number_check "$mm" "^[0-9]{2}$" 1 12; then
	return $FAIL
    fi
    # check YYYY is a year between 2016 and 2100 (a later date)
    if ! number_check "$YYYY" "^[0-9]{4}$" 2016 2100; then
	return $FAIL
    fi
    # check HHMMSS is a set of 6 numbers
    if ! string_check "$HHMMSS" "^[0-9]{6}$"; then
	return $FAIL
    fi
    # check HH is an hour between 00 and 23 (zero padded)
    if ! number_check "$HH" "^[0-9]{2}$" 0 23; then
	return $FAIL
    fi
    # check MM is a minute between 01 and 59 (zero padded)
    if ! number_check "$MM" "^[0-9]{2}$" 0 59; then
	return $FAIL
    fi
    # check SS is a second between 01 and 59 (zero padded)
    if ! number_check "$SS" "^[0-9]{2}$" 0 59; then
	return $FAIL
    fi
    return $PASS
}

get_size()
{
    # get property in column $1 of disk which contains the $BACKUP_DIR (in my case, the externarl hard drive)
    df -h -BG --output="$1" "$BACKUP_DIR" | tail -1 | grep -o "[0-9]\+" | head -1
}

########## check that everything is ready to perform backup

# check that script executed as root
if (( $(id -u) != 0 )); then
    echoerr "Sorry, must be root."
    echoerr "$ABORT_MSG"
    exit $BACKUP_DIR_NOT_FOUND # backup directory not found
fi

# check that backup directory exists
if ! isdir "$BACKUP_DIR"; then
    echoerr "Error: $BACKUP_DIR is not a directory"
    echoerr "Hint: check that backup external disk is connected and mounted at /root/backup/"
    echoerr "$ABORT_MSG"
    exit $BACKUP_DIR_NOT_FOUND # backup directory not found
fi

# check that backup directory has enough space
SPACE_AVAIL_BEFORE=$(get_size "avail")
SPACE_USED_BEFORE=$(get_size "used")
if [[ $SPACE_AVAIL_BEFORE -le $CRITICAL_SPACE_LEFT ]]; then
    echoerr "Only $SPACE_AVAIL_BEFORE GB left on backup media, which is less than $CRITICAL_SPACE_LEFT"
    echoerr "$ABORT_MSG"
    exit $NOT_ENOUGH_SPACE
fi

# process flags (sets prefix and, optionally, depth and monitoring mode)
DEPTH=3 # how many snapshots to keep (default)
WITH_MONITORING=false # if true, then recursively back up any unfinished backups
while getopts :d:p:m option
do
    if [[ $option == d ]]; then
	if [[ $OPTARG -gt 99 ]]; then
	    echoerr "Cannot make more than 100 snapshots per backup level"
	    echoerr "Value provided for -d flag cannot exceed 99"
	    echoerr "$ABORT_MSG"
	    exit $EXCESS_DEPTH
	fi
	DEPTH=$OPTARG
	echo "Keeping a $DEPTH snapshot history"
    elif [[ $option == p ]]; then
	for element in "${LEVELS[@]}"
	do
	    if [[ "$element" == "$OPTARG" ]]; then
		PREFIX=$OPTARG
		echo "This will be an $PREFIX backup"
		break
	    fi
	done
    elif [[ $option == m ]]; then
	echo "Monitoring enabled (will finish unfinished backup jobs, except ${LEVELS[0]} which will just be deleted)"
	WITH_MONITORING=true
    else
	echoerr "Unknown flag -$OPTARG"
	echoerr "$ABORT_MSG"
	exit $UNKNOWN_FLAG
    fi
done
if [[ -z $PREFIX ]]; then
    echoerr "Must specify flag -p with possible values [hourly/daily/weekly/monthly]"
    echoerr "$ABORT_MSG"
    exit $UNKNOWN_FLAG
fi

########## rotate $PREFIX snapshots

# step 1: make sure that every snapshot directory has the format NN.prefix_ddmmYYYY_THHMMSS
ALL_PREFIX=( $(find "$BACKUP_DIR" -maxdepth 1 -type d | grep -o '[^/]\{1,\}$') )
for CURRENT_PREFIX in "${ALL_PREFIX[@]}"
do
    if ! check_name_format "$CURRENT_PREFIX"; then
	# folder is not in correct name format
	# continuing could cause trouble, exit and let user take care of it manually!
	echoerr "$CURRENT_PREFIX is not in the naming format NN.prefix_ddmmYYYY_THHMMSS"
	echoerr "Continuing could cause trouble - please take care of this manually"
	echoerr "$ABORT_MSG"
	exit $BAD_BACKUP_DIR
    fi
done

# step 2: go through all current snapshots to verify that their rsync processes ran to completion
if $WITH_MONITORING; then
    declare -A UNDONE # indicator whether a level of backup was not finished
    for level in "${LEVELS[@]:1}"
    do
	UNDONE[$level]=false
    done
    UNFINISHED_BACKUPS_EXIST=false
else
    # if not in monitoring mode, go through only the $PREFIX (i.e. same level) backups
    ALL_PREFIX=( $(find "$BACKUP_DIR" -maxdepth 1 -type d -name "*""$PREFIX""*" | grep -o '[^/]\{1,\}$') )
fi
for CURRENT_PREFIX in "${ALL_PREFIX[@]}"
do
    BACKUP_RUNTIME_FILE=".running_$CURRENT_PREFIX"
    if [[ -f "$BACKUP_DIR$BACKUP_RUNTIME_FILE" ]]; then
	# existence of the $BACKUP_RUNTIME_FILE file tells that the backup in $CURRENT_PREFIX did not
	# run to completion. Therefore we shall remove this "corrupted" backup!
	THIS_LEVEL=$( echo "$CURRENT_PREFIX" | cut -d _ -f 1 | cut -d . -f 2 )
	if [[ "$THIS_LEVEL" == "$PREFIX" ]]; then
	    # this is an unfinished $PREFIX backup, can remove it as we're about to run a new $PREFIX backup
	    echoerr "Warning: $CURRENT_PREFIX backup did not run to completion (symptom: $BACKUP_RUNTIME_FILE exists)"
	    echoerr "Warning: Deleting $CURRENT_PREFIX backup..."
	    rm -rf "$BACKUP_DIR$CURRENT_PREFIX"
	    rm -rf "$BACKUP_DIR$BACKUP_RUNTIME_FILE"
	fi
	if $WITH_MONITORING; then
	    if ! -z ${UNDONE[$THIS_LEVEL]}; then # $THIS_LEVEL corresponds to backup type that we can auto-redo in monitoring mode
		if ! ${UNDONE[$THIS_LEVEL]}; then # if not already set to redo
		    # save a note that this level has an unfinished backup
		    echo "Detected that $THIS_LEVEL backup has been unfinished, will redo it"
		    UNDONE[$THIS_LEVEL]=true
		    UNFINISHED_BACKUPS_EXIST=true
		fi
	    fi
	fi
    fi
done

# step 3: regularize the numerical steps for $PREFIX backups (e.g. 00->02 becomes 00->01 to have regular count)
ALL_PREFIX=( $(find "$BACKUP_DIR" -maxdepth 1 -type d -name "*""$PREFIX""*" | grep -o '[^/]\{1,\}$' | sort -t_ -k2.5nr,2 -k2.3nr,2 -k2.1nr,2 -k3r) )
BACKUP_COUNT=-1 # indicator of how many backups the $PREFIX backup level has
for CURRENT_PREFIX in "${ALL_PREFIX[@]}"
do
    ((BACKUP_COUNT++))
    i=$((10#${CURRENT_PREFIX:0:2}))
    if [[ $i -ne $BACKUP_COUNT ]]; then
	REGULARIZED_PREFIX=$(printf "%02d" $BACKUP_COUNT)${CURRENT_PREFIX:2}
	echo "Warning: changing $CURRENT_PREFIX folder name to $REGULARIZED_PREFIX (irregular enumeration)"
	mv "$BACKUP_DIR$CURRENT_PREFIX" "$BACKUP_DIR$REGULARIZED_PREFIX"
    fi
done

# step 4: do the rotation
ALL_PREFIX=( $(find "$BACKUP_DIR" -maxdepth 1 -type d -name "*""$PREFIX""*" | grep -o '[^/]\{1,\}$' | sort -r) ) # descending order (treat oldest backups first)
for CURRENT_PREFIX in "${ALL_PREFIX[@]}"
do
    i=$((10#${CURRENT_PREFIX:0:2}))
    if [[ $i -ge $((DEPTH-1)) ]]; then
	# respect depth: remove backups that are too old
	echo "Backup depth: removing $CURRENT_PREFIX"
	rm -rf "$BACKUP_DIR$CURRENT_PREFIX"
    else
	# rotate the intermediate backups
	ROTATE_PREFIX=$(printf "%02d" $((++i)))${CURRENT_PREFIX:2}
	echo "Rotating: $CURRENT_PREFIX becomes $ROTATE_PREFIX"
	mv "$BACKUP_DIR$CURRENT_PREFIX" "$BACKUP_DIR$ROTATE_PREFIX"
    fi
done

########## create most recent backup now

NEW_PREFIX="00.""$PREFIX""_"$DATE
RSYNC_ARGS=(-aH --stats --delete --exclude={"/dev/*","/proc/*","/sys/*","/tmp/*","/run/*","/mnt/*","/media/*","/lost+found","/root/backup/*"})
BACKUP_RUNTIME_FILE=".running_$NEW_PREFIX"

# step 1: find most recent existing backup
MOST_RECENT=$(find "$BACKUP_DIR" -maxdepth 1 -type d | grep -o '[^/]\{1,\}$' | sort -t_ -k2.5nr,2 -k2.3nr,2 -k2.1nr,2 -k3r | head -1)
mkdir "$BACKUP_DIR$NEW_PREFIX" # folder where backup will be stored
touch "$BACKUP_DIR$BACKUP_RUNTIME_FILE" # file which exists while the backup is running

# step 2: make backup using rsync
if [[ -z $MOST_RECENT ]]; then
    echo "No existing backup, so creating a new backup of $SOURCE_DIR in $NEW_PREFIX"
    echo
    echo "------- rsync summary:"
    rsync "${RSYNC_ARGS[@]}" "$SOURCE_DIR" "$BACKUP_DIR$NEW_PREFIX"
else
    echo "Create backup of $SOURCE_DIR in $NEW_PREFIX, hard linked to $MOST_RECENT"
    echo
    echo "------- rsync summary:"
    rsync "${RSYNC_ARGS[@]}" --link-dest=../"$MOST_RECENT"/ "$SOURCE_DIR" "$BACKUP_DIR$NEW_PREFIX"
fi
RSYNC_EXIT_STATUS=$?
SPACE_AVAIL_AFTER=$(get_size "avail")
SPACE_USED_AFTER=$(get_size "used")
SPACE_PCENT_AFTER=$(get_size "pcent")
SPACE_SIZE=$(get_size "size")
echo
echo "Backup media total size:                  $SPACE_SIZE GB"
echo "Space available on media now:             $SPACE_AVAIL_AFTER GB"
echo "Space used on media now:                  $SPACE_USED_AFTER GB"
echo "Percentage of space used on backup media: $SPACE_PCENT_AFTER %"
echo
echo "This backup occupied *roughly* an extra "$((SPACE_USED_AFTER-SPACE_USED_BEFORE))" GB of space"
echo 
echo "-------"
echo
if [[ $RSYNC_EXIT_STATUS -ne 0 ]]; then
    # something went wrong (rsync returned non-zero exit code)
    echoerr "Something went wrong (rsync exit status ""$RSYNC_EXIT_STATUS"")."
    echoerr "$ABORT_MSG"
    exit $RSYNC_FAIL
fi

########## successful backup, closing actions

rm -rf "$BACKUP_DIR$BACKUP_RUNTIME_FILE" # delete runtime file, indicating that backup ran to completion
echo "Successful backup finished (rsync exist status ""$RSYNC_EXIT_STATUS"")."
echo "To safely see the backups (i.e. in read-only mode), execute:"
echo "sudo mount localhost:/root/backup /mnt/backup"
echo "(This creates a read-only NFS shared folder)"

########## run unfinished previous backups

if $WITH_MONITORING; then
    if $UNFINISHED_BACKUPS_EXIST; then
	echo
	echo "############################################################"
	echo "Running previously unfinished backups (since -m flag passed)"
	echo "############################################################"
	for level in "${LEVELS[@]:1}"
	do
	    if ${UNDONE[$level]}; then
		# run backup for the unfinished $level backup
		# since at least one such backup was deleted previously, guaranteed not to exceed depth,
		# thus the depth can be set to 99
		echo
		echo "#################### $level backup"
		echo
	        $0 -p "$level" -d 99
	    fi
	done
    fi
fi

exit $SUCCESS
