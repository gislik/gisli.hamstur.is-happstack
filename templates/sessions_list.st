<h1>Sessions</h1>
<h2><a href="/sessions/clean">Delete all</a></h2>
<table>
<tr>
<th>User</th><th>Session ID</th><th>Action</th>
</tr>
$sessions:{session |
<tr>
<td>$session.1.user.name$</td><td>$session.0$</td><td><a href="/sessions/delete/$session.0$">Delete</a></td>
</tr>
}$
</table>