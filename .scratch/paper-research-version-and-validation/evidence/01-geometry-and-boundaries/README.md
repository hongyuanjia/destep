# Ticket 01 验收记录

2026-10-08。“几何与边界转换验收”已完成。审查基线为 `e4d52689b7e1aefaff57e6587e603143a399ba62`；完整论文研究版本尚未冻结。

## 修改与工程含义

- 保留源模型的室外、地面和房间间边界关系，移除典型层循环重接及按倍率切断为绝热面的旧规则。倍率不同的相邻房间保留连接，通过警告与 `attr(idf, "conversion")$surface_boundaries` 暴露适用限制；不宣称这种倍率表达保证两侧年度热量等价。
- 对凹面及房间交汇处采用直接分割和有界回退；能够保留的凸面保留原状，确有闭合需要时才修复。检查分割面的覆盖、面积、方向、构造和相邻关系。
- 门使用门自身源面的热工信息，宿主用于定位和关联。开口按源宿主、平面及空间覆盖独立匹配，核对宿主内包含关系、对侧及覆盖完整性。
- 新增只读源引用校验。`MAIN_ENCLOSURE` 的 `SIDE1`、`SIDE2` 或 `MIDDLE_PLANE` 引用缺失时，公开 `to_idf()` 返回 `destep_invalid_surface_references` 错误，包含围护结构编号、字段及缺失引用；替代无法定位源问题的下标错误。
- 删除几何实现不再使用的 `polyclip` 依赖及相关登记。未增加兼容层。

完整研究工作区还包含得热、通风、遮阳及窗归属恢复等独立修改。这些实现和项目 README 修改未纳入本票提交。本票核验最终开口几何与宿主关系，但不把独立窗归属恢复实现并入本次几何提交。

## 源码与证据范围

两套验证对象分别登记：

1. **几何提交候选包**：从基线提取本票 24 个包文件；共享文件仅保留本票修改。完整 `R CMD check` 对该候选包执行。[candidate-manifest.json](candidate-manifest.json) 记录受检、暂存及打包后文件的 SHA-256。CSV 仅由 Git 正规化 CRLF 为 LF；`DESCRIPTION` 仅由 R 打包重新排版并增加标准元数据。其余受检文件与暂存内容一致。
2. **完整研究工作区**：三个原型的公开转换及独立几何审计使用完整研究实现。源码身份见 [research-source-manifest.csv](research-source-manifest.csv)，案例、源文件及生成 IDF 身份见 [conversion-results.csv](conversion-results.csv)。候选包的几何、边界、分割及门实现与研究工作区一致，但共享入口和其他子系统存在差异；原型审计不代表本次几何提交能够单独复现整篇论文。

实施前状态与差异保存在本地 `research/ticket-01-geometry-20261008/before-*`。实施期间已有文件的内容变化限定为边界校验、入口文档、生成帮助文件、NEWS 和测试辅助注释；其他既有研究修改均保留。

## 代表案例

公开入口生成 EnergyPlus 26.1 IDF。独立审计直接读取源 SQLite 和最终 IDF，以源顶点、平面与 Shapely 运算检查，不调用转换器几何函数。

| 案例 | 源面 | 目标面 | 源开口侧 | 检查项 | 既有朝向差异 | 新增异常／覆盖缺口 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| LE2-GoA-Changchun-2015 | 371 | 509 | 45 | 14,645 | 11 | 0／0 |
| LE2-Sch-Changchun-2015 | 129 | 165 | 21 | 4,567 | 0 | 0／0 |
| LE2-Inp-Changchun-2015 | 615 | 747 | 69 | 22,929 | 0 | 0／0 |
| 合计 | 1,115 | 1,421 | 135 | 42,141 | 11 | 0／0 |

三个最终 IDF 均通过对象校验。GoA、Sch、Inp 分别比较全部 141、140、140 张源表，共 421 张；表清单、内容及源文件 SHA-256 均保持不变。不同倍率的房间间连接分别为 51、0、54 对，并有显式审计记录。

11 项朝向差异沿用历史第十三轮证据的明确边界，仅允许域、源键、字段、源值、目标值及容差均与历史记录完全一致的条目；没有把它们计作通过或修复。摘要见 [geometry-summary.json](geometry-summary.json)。完整本地证据为 `research/ticket-01-geometry-20261008/conversion-final/`，历史依据为 `research/paper-restart-20261006/THIRTEENTH-PASS.md` 及其对应 `source-failures.csv`。

## 异常输入与回归

- 先在旧基线上观察公开倍率／边界回归失败；新增引用错误测试也先观察到旧实现的下标错误，再实现定位诊断。最终新增公开测试 31 项断言通过，覆盖三个引用字段、调用者全部表及源文件保持情况，见 [public-regression.log](public-regression.log)。
- 独立异常输入验证拒绝缺失的相邻面和中间平面，同时保持调用者全部表与源文件不变，见 [negative-results.csv](negative-results.csv)。
- 审计重跑开始即清除自身旧成功标记。优化 Python 会使历史审计断言失效，因此明确拒绝 `-O`；已验证拒绝时旧标记消失，随后正常重跑重新产生成功摘要，见 [optimized-negative.log](optimized-negative.log)。
- 候选包完整检查：**R CMD check Status: OK；FAIL 0、WARN 27、SKIP 19、PASS 1830**。27 条测试警告涉及已知转换边界，19 项跳过因缺少专用 HVAC 源案例，不计作通过。日志末尾另有数据库连接清理提示，未影响检查返回状态，保留供后续资源管理核查。详见 [check.log](check.log) 和 [testthat.Rout](testthat.Rout)。提交的日志副本仅清除行末空格，原始日志仍保存在本地研究目录。
- 完整套件包含既有 EnergyPlus 23.1 一日几何回归。本票未新增年度模拟；输入审计与测试通过不构成年度能耗、峰值或逐时数值等价证据。
- 本票所有提交的 R 文件通过项目 Air 格式检查；roxygen 文档由源注释生成。

## Standards

独立代码规范复审完成，无剩余阻塞项。内部命名、四空格缩进、解释性注释和只读诊断符合项目规则；复审中发现的重复审计赋值、旧成功标记和异常输入保持检查范围均已修正并重新验证。

## Spec

独立规格复审完成，无剩余阻塞项。成功与异常公开入口、独立开口对侧关系、源数据保持及源码／案例身份均有证据。既有朝向差异和两套验证对象的边界保持可见。

## 本地重跑

脚本及完整日志位于 `research/ticket-01-geometry-20261008/`，使用现有 uvr R 4.6.0 与 uv 审计环境。`generate.R` 和 `check.R` 拒绝覆盖既有输出；重跑时先指定新输出目录并同步 `audit.py` 的输入目录，保留本次证据。

```sh
R_PROFILE_USER=/dev/null uvr run --r-version '=4.6.0' research/ticket-01-geometry-20261008/test-focused.R
uv run --offline --no-sync --project research/paper-restart-20261006/seventh-pass python research/ticket-01-geometry-20261008/audit.py
```

`generate.R`、`negative.R`、`check.R` 均通过相同 uvr R 版本运行；`check.R` 的候选源码目录需按当前检出位置设置。生成脚本只写 IDF 和紧凑审计文件，不请求年度模拟或新增 ESO／结果 SQLite。原型重跑仍依赖本地源模型与历史审计模块，可移植复现包由 ticket-10 处理。

后续可执行 ticket-02 与 ticket-03；ticket-04 仍需二者完成后才能冻结研究版本。
