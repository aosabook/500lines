/**
  *  analysis.als
  *    A module that puts everything together
  *    End-to-end analysis of security properties
  */
module analysis

open flow
open sop

// General security properties
sig TrustedModule, MaliciousModule in FlowModule {}
sig PrivateData, MaliciousData in Data {}

assert Confidentiality {
  -- No malicious agent should be able to access private data
  no m : MaliciousModule, t : Time | 
    some PrivateData & m.accesses.t
}

assert Integrity {
  -- No malicious data should ever flow into a trusted module
  no u : TrustedModule, t : Time | 
    some MaliciousData & u.accesses.t
}

check Confidentiality for 5
check Integrity for 5

// Helper functions for visualization
fun currentCall : Call -> Time {
  {c : Call, t : Time | c.start = t }
}

