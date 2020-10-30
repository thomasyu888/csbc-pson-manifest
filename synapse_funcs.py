import synapseclient

syn = synapseclient.Synapse()
syn_login = syn.login
syn_user_profile = syn.getUserProfile
syn_table_query = syn.tableQuery
syn_get = syn.get
syn_store = syn.store

