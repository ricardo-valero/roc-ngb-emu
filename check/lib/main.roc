# Check infrastructure, deliberately outside the emulator package: verdict
# protocols for test ROMs and the digest used by golden checks.
package [Harness, Sha256, Inflate, Tar] { ngb: "../../package/main.roc" }

import Harness
import Inflate
import Sha256
import Tar
