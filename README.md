# RustVerifier

## Core Syntax

$$
\begin{align*}
    \text{Lifetime, } \kappa ::= & \textbf{static} \mid \alpha \\
    \text{Access Modifier, } \mu ::= & \textbf{mut} \mid \textbf{shr} \\
    \text{L-Value, } \overset{\leftharpoonup}{v} ::= & x \mid * \overset{\leftharpoonup}{v}\\
    \text{Value, } v ::= & z \mid true \mid false \mid \ell \\
    \text{Type} \ni \tau ::= & \textbf{bool} \mid \textbf{int} \mid \square \; \tau
        \mid \&^{\mu} \tau \mid \langle \tau \rangle^{\kappa} \mid () \\
    \text{Lifetime Context, } \Psi ::= & (\mathcal{L}, \preceq) \\
    \text{Expression} \ni e ::= &\: v \mid \text{let mut } x = e
        \mid \text{move } \overset{\leftharpoonup}{v} \mid \text{copy } \overset{\leftharpoonup}{v}  \\
        & \mid \overset{\leftharpoonup}{v} = e \mid \text{box } e \mid \& \overset{\leftharpoonup}{v} \mid \& \text{mut } \overset{\leftharpoonup}{v} \\
        & \mid \text{endLifetime } x \\
        & \mid e_1 + e_2 \mid e_1 - e_2
        \mid e_1 \leq e_2 \mid e_1 == e_2  \\
        &\mid \{ e_1; e_2; ...; e_n \}
\end{align*}
$$

## Hoare Forward Rules

$$
% int rule
\frac{}{\{ \Psi, \Delta \} \; z \; \{ \textbf{ens}[r]{ \Psi, \Delta \land r:\mathbf{int}}\}} \quad \text{(int)}
$$

$$
% true rule
\frac{}{\{ \Psi, \Delta \} \; true \; \{ \textbf{ens}[r]{ \Psi, \Delta \land r:\mathbf{bool}}\}} \quad \text{(true)}
$$

$$
% false rule
\frac{}{\{ \Psi, \Delta \} \; false \; \{ \textbf{ens}[r]{ \Psi, \Delta \land r:\mathbf{bool}}\}} \quad \text{(false)}
$$

$$
% box rule
\frac{\{ \Psi, \Delta \} \; e \; \{ \textbf{ens}[r_1]{ \Psi, \Delta \land r_1:\tau}\}}
{\{ \Psi, \Delta \} \; \text{box } e \; \{ \textbf{ens}[r_2]{ \Psi, \Delta \land r_2:\square \; \tau }\}} \quad \text{(box)}
$$

$$
% let-create-lifetime rule
\frac{\textit{fresh } \texttt{'a} \quad \{ \Psi, \Delta \} \; e \; \{ \textbf{ens}[r_1]{ \Psi, \Delta \land r_1 : \tau }\} \quad \neg \text{RefType} (\tau) \quad \Psi_1 \equiv (\mathcal{L} \cup \{ \texttt{'a} \}, \preceq)}
{\{ (\mathcal{L}, \preceq), \Delta \} \; \text{let mut } x = e \; \{ \textbf{ens}[r_2]{ \Psi_1, \Delta \land x: \langle \tau \rangle^{\texttt{'a}} \land r_2 : () }\}} \quad \text{(let-create-lifetime)}
$$

$$
% let-propagate-lifetime rule
\frac{\{ \Psi, \Delta \} \; e \; \{ \textbf{ens}[r_1]{ \Psi_1, \Delta_1 }\}}
{\{\Psi, \Delta \} \; \text{let mut } x = e \; \{ \textbf{ens}[r_2]{ \Psi_1, [r_1 := x]\Delta_1 \land r_2 : () }\}} \quad \text{(let-propagate-lifetime)}
$$

$$
% de-reference rule
\frac{\Delta \vdash \overset{\leftharpoonup}{v} : \langle \square \; \tau \rangle^{\texttt{'a}} \lor \overset{\leftharpoonup}{v} : \langle \&^{\mu} \tau \rangle^{\texttt{'a}}}
{\Delta \vdash * \overset{\leftharpoonup}{v} : \langle \tau \rangle^{\texttt{'a}}} \quad \text{(de-reference)}
$$

$$
% endlifetime rule
\frac{\Delta \vdash x: \langle \tau \rangle^{\texttt{'a}} \quad \texttt{'a} \in \mathcal{L} \quad \Psi_1 \equiv (\mathcal{L} \setminus \{ \texttt{'a} \}, \preceq)}
{\{ (\mathcal{L}, \preceq), \Delta \} \; \text{endLifetime } {x}\; \{ \textbf{ens}[r]{ \Psi_1, \Delta \land r: ()} \}} \quad \text{(endlifetime)}
$$

$$
% move rule
\frac{\textit{fresh } \texttt{'b} \quad \neg \text{readProhibited}(\Psi, \Delta, x, \texttt{'a}) \quad \Psi_1 \equiv ((\mathcal{L} \setminus \{ \texttt{'a} \}) \cup \{ \texttt{'b} \}, \preceq)}
{\{ (\mathcal{L}, \preceq), \Delta \} \; \text{move } x \; \{ \textbf{ens}[r]{ \Psi_1, \Delta \land r: \langle \tau \rangle^{\texttt{'b}} }\}} \quad \text{(move)}
$$

$$
% immutable-borrow rule
\frac{\textit{fresh } \texttt{'b} \quad \Delta \vdash \overset{\leftharpoonup}{v}: \langle \tau \rangle^{\texttt{'a}} \quad \neg \text{readProhibited}(\Psi, \Delta, \overset{\leftharpoonup}{v}, \texttt{'a}) \quad \Psi_1 \equiv (\mathcal{L} \cup \{ \texttt{'b} \}, \preceq \cup \{ (\texttt{'a, 'b}) \})}
{\{ (\mathcal{L}, \preceq), \Delta \} \; \& \overset{\leftharpoonup}{v} \; \{ \textbf{ens}[r]{ \Psi_1, \Delta \land r: \langle \&^{\textbf{shr}} \tau \rangle^{\texttt{'b}}} \}} \quad \text{(shr-borrow)}
$$

$$
% mutable-borrow rule
\frac{\textit{fresh } \texttt{'b} \quad \Delta \vdash \overset{\leftharpoonup}{v}: \langle \tau \rangle^{\texttt{'a}} \quad \neg \text{writeProhibited}(\Psi, \Delta, \overset{\leftharpoonup}{v}, \texttt{'a}) \quad \Psi_1 \equiv (\mathcal{L} \cup \{ \texttt{'b} \}, \preceq \cup \{ (\texttt{'a, 'b}) \})}
{\{ (\mathcal{L}, \preceq), \Delta \} \; \&\text{mut } \overset{\leftharpoonup}{v} \; \{ \textbf{ens}[r]{ \Psi_1, \Delta \land r: \langle \&^{\textbf{mut}} \tau \rangle^{\texttt{'b}}} \}} \quad \text{(mutable-borrow)}
$$

$$
% assignment rule
\frac{\Delta \vdash \overset{\leftharpoonup}{v}: \langle \tau \rangle^{a} \quad \{ \Psi, \Delta \} \; e \; \{ \textbf{ens}[r_1]{ \Psi, \Delta \land r_1:\tau}\} \quad \neg \text{writeProhibited}(\Psi, \Delta, \overset{\leftharpoonup}{v}, a)}
{\{ (\mathcal{L}, \preceq), \Delta \} \; \overset{\leftharpoonup}{v} = e \; \{ \textbf{ens}[r_2]{ \Psi, \Delta \land r_2:() }\}} \quad \text{(assignment)}
$$
