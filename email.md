
# Table of Contents

1.  [Tools I used for my workflow](#orgdd51d8e)
2.  [Syncing Notmuch Tags with IMAP Folders: Workarounds for Cross-Device Email Management](#org8b44756)
    1.  [The shell script to move mail between Maildir folders](#org1c1823b)
    2.  [Notmuch config to incorporate the above movemail script](#orgb73e3d7)
        1.  [Example pre-new script](#orgddfd32e)
        2.  [Example post-new script](#orgc54764a)
        3.  [Explanation](#org2abf381)
        4.  [Example `$HOME/.notmuch-config` File](#orgba299d1)
    3.  [Emacs configuration](#org5aae490)
    4.  [Full Command for Syncing Emails](#org2560497)
3.  [Configure oauth2 support with outlook](#org8716f10)
    1.  [Use the mutt script for oauth2 authentication](#org47d6a2a)
    2.  [Set up app registration in Azure](#org10ff27e)
    3.  [Fetch the authentication token with the mutt script](#org3339a5a)
    4.  [Configure mbsync](#orgb8bb919)
    5.  [Configure msmtp](#orgff3c6df)
4.  [From `mu4e` to `notmuch`](#org70f995a)
5.  [Acknowledgement](#orgb4ace16)

If you&rsquo;re currently reading this README file in Markdown format, it
has been generated through `org-export`, from its original org
format. For the best experience, please consider reading the org
format file instead.


<a id="orgdd51d8e"></a>

# Tools I used for my workflow

My email workflow mainly consists of the following parts:

1.  Email synchronizer (indexer): `mbsync` (or `isync`, they are the same thing)
2.  Email Sender: `msmtp`
3.  Mail User Agent (MUA): `notmuch` and `emacs`
4.  `mutt_oauth2.py` from [neomutt](https://github.com/muttmua/mutt/blob/master/contrib/mutt_oauth2.py) for oauth2 authentication with outlook email.

For how `mbsync + notmuch + msmtp` above work together, [Protesilaos
Stavrou](https://protesilaos.com/emacs/dotemacs#h:33151014-3621-4be6-9185-aacc34ee2d2c) provides a great explanation.

I will not delve into the configuration of `mbsync` and `msmtp` here, as
abundant resources are available across the internet. Instead, I&rsquo;ll
focus on aspects less commonly discussed online: specifically, how to
set up notmuch effectively and configure OAuth2 authentication with
mbsync.

This guide will address two key areas:

1.  My hacks on `notmuch` for efficient email management
2.  Configuring OAuth2 authentication for `mbsync`


<a id="org8b44756"></a>

# Syncing Notmuch Tags with IMAP Folders: Workarounds for Cross-Device Email Management

Notmuch is a tag-centric email client, but its tags don&rsquo;t usually sync
up with IMAP folders. It also can&rsquo;t move emails between maildir
folders (which are basically mailboxes in your email account) out of
the box. This setup works fine if you only use Notmuch to read your
emails. However, things can get messy if you&rsquo;re also using other email
clients, like on your phone, and want everything to stay in sync
across your devices.

This section covers some handy workarounds, mainly focusing on moving
emails between maildir folders. These tricks help sync changes with
your email mailboxes, making it easier to use Notmuch alongside other
email clients on different devices without things getting out of sync.


<a id="org1c1823b"></a>

## The shell script to move mail between Maildir folders

Put the following file under your `$PATH`, and give it a name like
`movemail`.

This script is adapted from [A discussion in notmuch mailing list](https://notmuchmail.org/pipermail/notmuch/2019/028956.html).

This script accepts two params, the first is the file path of your
email, and the second is the Maildir folder. Typically the first
parameter will be piped from stdin in a notmuch hook, and the second
will be the targeted maildir destination.

    #!/usr/bin/env bash
    # move mails stored in maildir format between maildir folders. Mainly used as a
    # pre-hook script executed at `notmuch new` command. This script also assumes
    # the maildir synchronizer is mbsync/isync.
    
    # `mbsync` requires an unique "UID" identifier for the filename of each mail.
    # so the HEADER U=xxx should be removed before moving mails.
    
    # remove the paths (leading characters), only containing the filename
    only_last_file_name=${1##*/} # double #s indicates matching chars as many as possible.
    
    # check if file if exists
    if [ ! -f "$1" ];
    then
        echo "File $1 does not exist."
        exit 0;
    fi
    
    # don't print the output of grep, just check the condition
    if echo $1 | grep ':2,[PRSTDF]\{1,6\}' > /dev/null;
    then
        # print out messages of moving files
        echo "Moving $1 to $2/cur/$(echo $only_last_file_name | sed 's/,U=[0-9]\{1,\}//')"
        # move messages with flags to cur/ directory
        mv -f "$1" "$2/cur/$(echo $only_last_file_name | sed 's/,U=[0-9]\{1,\}//')"
    else
        # print out messages of moving files
        echo "Moving $1 to $2/new/$(echo $only_last_file_name | sed 's/,U=[0-9]\{1,\}//')"
        # move messages with no flags to new/ directory
        mv -f "$1" "$2/new/$(echo $only_last_file_name | sed 's/,U=[0-9]\{1,\}//')"
    fi


<a id="orgb73e3d7"></a>

## Notmuch config to incorporate the above movemail script

The integration of the movemail script requires configuration of two Notmuch hooks:

1.  pre-new Hook
    -   Executes before `notmuch new` command
    -   Manages email migration between Maildir folders

2.  post-new Hook
    -   Executes after `notmuch new` command
    -   Updates Notmuch tags to reflect new email locations
    -   Update tags with corresponding Maildir folders


<a id="orgddfd32e"></a>

### Example pre-new script

    #!/usr/bin/env bash
    
    my_personal_gmail="$HOME/Maildir/my-personal-gmail"
    my_personal_outlook="$HOME/Maildir/my-personal-outlook"
    
    # action-delete
    notmuch search --output=files --format=text0 "tag:action-delete and folder:/my-personal-gmail/" \
        | xargs -r -0 -n1 -I{} movemail {} "$my_personal_gmail/[Gmail]/Trash"
    
    notmuch search --output=files --format=text0 "tag:action-delete and folder:/my-personal-outlook/" \
        | xargs -r -0 -n1 -I{} movemail {} "$my_personal_outlook/Deleted"
    
    # action-archive
    notmuch search --output=files --format=text0 "tag:action-archive and folder:/my-personal-gmail/" \
        | xargs -r -0 -n1 -I{} movemail {} "$my_personal_gmail/Archive"
    
    notmuch search --output=files --format=text0 "tag:action-archive and folder:/my-personal-outlook/" \
        | xargs -r -0 -n1 -I{} movemail {} "$my_personal_outlook/Archive"
    
    # action-spam
    notmuch search --output=files --format=text0 "tag:action-spam and folder:/my-personal-gmail/" \
        | xargs -r -0 -n1 -I{} movemail {} "$my_personal_gmail/[Gmail]/Spam"
    
    notmuch search --output=files --format=text0 "tag:action-spam and folder:/my-personal-outlook/" \
        | xargs -r -0 -n1 -I{} movemail {} "$my_personal_outlook/Junk"
    
    # action-inbox
    notmuch search --output=files --format=text0 "tag:action-inbox and folder:/my-personal-gmail/" \
        | xargs -r -0 -n1 -I{} movemail {} "$my_personal_gmail/Inbox"
    
    notmuch search --output=files --format=text0 "tag:action-inbox and folder:/my-personal-outlook/" \
        | xargs -r -0 -n1 -I{} movemail {} "$my_personal_outlook/Inbox"


<a id="orgc54764a"></a>

### Example post-new script

    #!/usr/bin/env bash
    # Synchronization between maildir folders and tag
    
    notmuch tag +sent -- folder:"/Sent/"
    notmuch tag -sent -- not folder:"/Sent/"
    
    notmuch tag +deleted -- folder:"/Trash/" or folder:"/Deleted/"
    notmuch tag -deleted -- not folder:"/Trash/" and not folder:"/Deleted/"
    
    notmuch tag +drafts -- folder:"/Drafts/" or folder:"/drafts/"
    notmuch tag -drafts -- not folder:"/Drafts/" and not folder:"/drafts/"
    
    notmuch tag +archive -- folder:"/Archive/"
    notmuch tag -archive -- not folder:"/Archive/"
    
    notmuch tag +spam -- folder:"/Spam/" or folder:"/Junk/"
    notmuch tag -spam -- not folder:"/Spam/" and not folder:"/Junk/"
    
    notmuch tag +inbox -- folder:"/Inbox/"
    notmuch tag -inbox -- not folder:"/Inbox/"
    
    notmuch tag -action-delete -- tag:deleted
    notmuch tag -action-archive -- tag:archive
    notmuch tag -action-spam -- tag:spam
    notmuch tag -action-inbox -- tag:inbox


<a id="org2abf381"></a>

### Explanation

The `pre-new` hook performs the following tasks:

-   Checks the tags of the current emails. If an email has an
    `action-delete` tag, it moves the email to the trash folder of the
    corresponding email account.
-   Applies the same logic for `action-archive`, `action-spam`, and
    `action-inbox`, moving emails to their respective folders.

The post-new hook performs two primary tag management tasks:

1.  Update tags based on location
    -   Assigns `sent` tag to emails in the Sent folder
    -   Removes `sent` tag from emails no longer in the Sent folder
    -   Repeat the sameting for other tags: archive, inbox, and spam.

2.  Clean up action based tags
    -   Removes action-related tags (deleted, archive, spam, inbox) once emails are moved
    -   Cleans up tags after successful folder transitions
    -   Prevents redundant actions on already-processed emails


<a id="orgba299d1"></a>

### Example `$HOME/.notmuch-config` File

    [database]
    path=Maildir
    hook_dir=.config/notmuch/hooks
    [user]
    name=Milan Glacier
    primary_email=xxx@outlook.com
    other_email=xxx@gmail.com
    [new]
    tags=new
    ignore=.mbsyncstate;.uidvalidity;.DS_Store;msmtp.log;
    [search]
    exclude_tags=deleted;spam;
    [maildir]
    synchronize_flags=true


<a id="org5aae490"></a>

## Emacs configuration

The following lines in my Emacs configuration reflect my tag setup:

    (setq notmuch-tagging-keys '(("a" notmuch-archive-tags "Archive")
                                 ("u" notmuch-show-mark-read-tags "Mark read")
                                 ("f" ("+flagged") "Flag")
                                 ("s" ("+action-spam") "Mark as spam")
                                 ("d" ("+action-delete") "Delete"))
          notmuch-archive-tags '("+action-archive")
          notmuch-draft-tags '("+drafts")
          mg-notmuch-deleted-tags "action-delete")

When I run the `notmuch-tag-jump` command, I can mark emails for specific action

-   `action-delete` to delete an email
-   `action-archive` to archive an email
-   `action-spam` to mark an email as spam

During synchronization, Notmuch moves emails tagged as `action-*` to the
corresponding folders and `mbsync` syncs the changes to the remote
host.


<a id="org2560497"></a>

## Full Command for Syncing Emails

    notmuch new && mbsync -a && notmuch new

-   The first `notmuch new` moves emails between Maildir folders.
-   The second `notmuch new` indexes new incoming emails and update the
    local notmuch database.


<a id="org8716f10"></a>

# Configure oauth2 support with outlook


<a id="org47d6a2a"></a>

## Use the mutt script for oauth2 authentication

Outlook enforced oauth2 authentication for outlook email. See this
[news](https://support.microsoft.com/en-us/office/modern-authentication-methods-now-needed-to-continue-syncing-outlook-email-in-non-microsoft-email-apps-c5d65390-9676-4763-b41f-d7986499a90d).

The [mutt<sub>oauth2.py</sub>](https://github.com/neomutt/neomutt/tree/main/contrib/oauth2) script, sourced from NeoMutt&rsquo;s contributions,
enables OAuth2 authentication. Earlier versions of this script
required direct modification—for example, to alter the GPG identity or
the encryption method for OAuth tokens. However, the most recent
version streamlines this process by allowing these settings to be
supplied as command-line arguments, thereby eliminating the need for
manual edits and enabling the script&rsquo;s direct use.


<a id="org10ff27e"></a>

## Set up app registration in Azure

To set up an app to sync emails, follow these steps:

1.  Sign up for an Azure account using your personal Outlook email or
    preferred email address.

2.  After logging in to your Azure account, switch to the &ldquo;default
    directory&rdquo; as new app registrations are currently restricted to
    directories only.

3.  Follow the [instruction](https://github.com/neomutt/neomutt/tree/main/contrib/oauth2#how-to-create-a-microsoft-registration) provided by neomutt for your app
    configuration. During the app configuration process, you&rsquo;ll find
    the specified settings under the `Manage | Authentication` and
    `Manage | API permissions` sidebars in your app portal. It&rsquo;s crucial
    to enable access for IMAP, SMTP, POP, and other relevant scopes as
    detailed in the mutt script under the `Manage | API permissions`
    sidebar.


<a id="org3339a5a"></a>

## Fetch the authentication token with the mutt script

    mutt_oauth2.py --authorize --verbose -t "your/path/to/the/oauth/file" \
            --provider microsoft --client-id "your_azure_client_id" \
            --encryption-pipe "gpg --recipient your@gpg.id.com --encrypt --always-trust" \
            --authflow devicecode

Upon executing the aforementioned command, a wizard will guide you
through the setup process. Follow the on-screen instructions to
proceed. I used the &rsquo;devicecode&rsquo; authentication flow, although other
options should also work. Once the setup is complete, the mutt script
will encrypt the OAuth2 token and save it to
`your/path/to/the/oauth/file`.

Please be aware that OAuth2 tokens expire regularly. I recommend
familiarize yourself with the concept of refresh tokens and access tokens in
the OAuth2 protocol. If you encounter issues with mbsync, consider the
possibility of token expiration. In such cases, you may need to execute the
command again to obtain a fresh token.


<a id="orgb8bb919"></a>

## Configure mbsync

**Warning**: if you are using macOS, the `mbsync` installed from `homebrew`
does not work with `xoauth2`, following the instruction at this
[thread](https://github.com/moriyoshi/cyrus-sasl-xoauth2/issues/9#issuecomment-2161796043)
to build the `mbsync` from source. I took my own hackish approach: I
created a Docker container that runs mbsync with xoauth2 support. I
then mounted my host machine&rsquo;s maildir folder to this container,
allowing mbsync to access and sync my emails. The Docker container is
based on Ubuntu and includes the following installed packages:

    isync libsasl2-modules-kdexoauth2 ca-certificates openssl libsasl2-modules python3

In your mbsync config file, configure your outlook email like this:

    IMAPAccount myPersonalOutlook
    PassCmd "mutt_oauth2.py -t your/path/to/the/outlook/oauth/file"
    AuthMechs XOAUTH2
    # And your rest configs just works


<a id="orgff3c6df"></a>

## Configure msmtp

You are lucky that the `msmtp` installed from `homebrew` just works, no
need to worry about building from source. Changing the following lines
should be sufficient.

    account outlook
    auth xoauth2
    passwordeval mutt_oauth2.py -t your/path/to/the/outlook/oauth/file
    # And your rest configs just works


<a id="org70f995a"></a>

# From `mu4e` to `notmuch`

As someone switching from `mu4e` to `notmuch`, I&rsquo;ve found that the
resources I used to set up mu4e are still super helpful for
configuring `notmuch`. It&rsquo;s great that I can keep using `mbsync` for
syncing my emails and msmtp for sending messages. These tools work
just as well with `notmuch`, making the transition much smoother.

There are two primary reasons for my decision to transition. Firstly,
mu4e&rsquo;s backward compatibility is horrible, with many plugins dependent
on mu4e often breaking after major version updates. Secondly, notmuch
offers a threaded conversation view, whereas mu4e only provides a
threaded tree view. While I had implemented thread-folding in mu4e, it
worked but the result was less than ideal. Even the official
thread-folding feature introduced in mu4e 1.12 has numerous issues. In
contrast, the conversation view in notmuch works great for threads.


<a id="orgb4ace16"></a>

# Acknowledgement

1.  [Protesilaos Stavrou&rsquo;s notmuch configuration](https://protesilaos.com/emacs/dotemacs#h:755e195b-9471-48c7-963b-33055969b4e2)
2.  [Doomemacs&rsquo;s mu4e configuration](https://github.com/doomemacs/doomemacs)
3.  [Doomemacs&rsquo;s notmuch configuration](https://github.com/doomemacs/doomemacs)
4.  [macowner&rsquo;s blog on mu4e config](https://macowners.club/posts/email-emacs-mu4e-macos/)
5.  [Move mail script from notmuch mailing list](https://notmuchmail.org/pipermail/notmuch/2019/028956.html)

